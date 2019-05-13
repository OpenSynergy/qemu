/*
 * Virtio Sound Device
 *
 * Copyright (C) 2019 OpenSynergy GmbH
 *
 * This work is licensed under the terms of the GNU GPL, version 2 or
 * (at your option) any later version.  See the COPYING file in the
 * top-level directory.
 */

#include "qemu/osdep.h"
#include "audio/audio.h"
#include "audio/audio_int.h"
#include "hw/virtio/virtio-access.h"
#include "sysemu/dma.h"
#include "virtio-snd.h"

static void viosnd_pcm_out_cb(void *data, int available)
{
    VirtIOSoundPCMStream *stream = data;
    VirtIOSoundPCMBlock *block;
    VirtIOSoundPCMBlock *next;

    qemu_mutex_lock(&stream->queue_mutex);
    QSIMPLEQ_FOREACH_SAFE(block, &stream->queue, entry, next) {
        int size = MIN(block->size, available);

        size = AUD_write(stream->voice.out, block->data + block->offset, size);
        block->size -= size;
        if (!block->size) {
            QSIMPLEQ_REMOVE(&stream->queue, block, VirtIOSoundPCMBlock, entry);
            g_free(block);
        } else {
            block->offset += size;
        }

        available -= size;
        if (!available) {
            break;
        }
    }
    qemu_mutex_unlock(&stream->queue_mutex);
}

static void viosnd_pcm_in_cb(void *data, int available)
{
    VirtIOSoundPCMStream *stream = data;
    VirtIOSoundPCMBlock *block;

    block = g_malloc(sizeof(VirtIOSoundPCMBlock) + available);
    block->size = AUD_read(stream->voice.in, block->data, available);
    block->offset = 0;

    qemu_mutex_lock(&stream->queue_mutex);
    QSIMPLEQ_INSERT_TAIL(&stream->queue, block, entry);
    qemu_mutex_unlock(&stream->queue_mutex);
}

static int viosnd_pcm_qemu_format(uint16_t format)
{
    switch (format) {
    case VIRTIO_SND_PCM_FMT_S8:
        return AUDIO_FORMAT_S8;
    case VIRTIO_SND_PCM_FMT_U8:
        return AUDIO_FORMAT_U8;
    case VIRTIO_SND_PCM_FMT_S16:
        return AUDIO_FORMAT_S16;
    case VIRTIO_SND_PCM_FMT_U16:
        return AUDIO_FORMAT_U16;
    case VIRTIO_SND_PCM_FMT_S32:
        return AUDIO_FORMAT_S32;
    case VIRTIO_SND_PCM_FMT_U32:
        return AUDIO_FORMAT_U32;
    default:
        return -1;
    };
}

#define VIO2QEMU_RATE(_value_) \
    [VIRTIO_SND_PCM_RATE_ ## _value_] = _value_

static int viosnd_pcm_qemu_rate(uint16_t rate)
{
    static const unsigned int map[] = {
        VIO2QEMU_RATE(8000),
        VIO2QEMU_RATE(11025),
        VIO2QEMU_RATE(16000),
        VIO2QEMU_RATE(22050),
        VIO2QEMU_RATE(32000),
        VIO2QEMU_RATE(44100),
        VIO2QEMU_RATE(48000),
        VIO2QEMU_RATE(64000),
        VIO2QEMU_RATE(88200),
        VIO2QEMU_RATE(96000),
        VIO2QEMU_RATE(176400),
        VIO2QEMU_RATE(192000)
    };

    return (rate < ARRAY_SIZE(map)) ? map[rate] : -1;
}

static inline VirtIOSoundPCMStream *
viosnd_pcm_stream(VirtIOSound *snd, struct virtio_snd_pcm_hdr *request)
{
    if (request->pcm_id == 0) {
        switch (request->stream_type) {
        case VIRTIO_SND_PCM_T_PLAYBACK:
        case VIRTIO_SND_PCM_T_CAPTURE:
            return snd->pcm->streams[request->stream_type];
        }
    }

    return NULL;
}

static inline bool viosnd_pcm_is_playback(VirtIOSoundPCMStream *stream)
{
    return stream->pcm->streams[VIRTIO_SND_PCM_T_PLAYBACK] == stream;
}

static void viosnd_pcm_flush(VirtIOSoundPCMStream *stream)
{
    while (!QSIMPLEQ_EMPTY(&stream->queue)) {
        VirtIOSoundPCMBlock *block = QSIMPLEQ_FIRST(&stream->queue);

        QSIMPLEQ_REMOVE_HEAD(&stream->queue, entry);
        g_free(block);
    }
}

static void viosnd_pcm_open(VirtIOSoundPCMStream *stream)
{
    if (viosnd_pcm_is_playback(stream)) {
        stream->voice.out = AUD_open_out(&stream->pcm->snd->card,
                                         stream->voice.out, "virtio_snd.out",
                                         stream,
                                         viosnd_pcm_out_cb,
                                         &stream->as);
    } else {
        stream->voice.in = AUD_open_in(&stream->pcm->snd->card,
                                       stream->voice.in, "virtio_snd.in",
                                       stream,
                                       viosnd_pcm_in_cb,
                                       &stream->as);
    }
}

static void viosnd_pcm_close(VirtIOSoundPCMStream *stream)
{
    if (viosnd_pcm_is_playback(stream)) {
        AUD_close_out(&stream->pcm->snd->card, stream->voice.out);
        stream->voice.out = NULL;
    } else {
        AUD_close_in(&stream->pcm->snd->card, stream->voice.in);
        stream->voice.in = NULL;
    }

    viosnd_pcm_flush(stream);
}

static void viosnd_pcm_set_active(VirtIOSoundPCMStream *stream, int active)
{
    if (viosnd_pcm_is_playback(stream)) {
        AUD_set_active_out(stream->voice.out, active);
    } else {
        AUD_set_active_in(stream->voice.in, active);
    }
}

static uint32_t viosnd_base_configuration(VirtIOSound *snd,
                                          struct virtio_snd_hdr *request,
                                          size_t req_size,
                                          struct virtio_snd_hdr *response,
                                          size_t resp_size)
{
    VirtIODevice *vdev = &snd->parent_obj;
    struct virtio_snd_base_configuration *cfg =
        (struct virtio_snd_base_configuration *)response;
    struct virtio_snd_pcm_desc *pcm_desc;
    struct virtio_snd_pcm_stream_desc *pcm_stream_desc;
    uint32_t length;
    int i;

    if ((req_size != sizeof(struct virtio_snd_hdr)) ||
        (resp_size != sizeof(struct virtio_snd_base_configuration))) {
        return VIRTIO_SND_S_EINVALID;
    }

    memset(cfg, 0, resp_size);

    pcm_desc = (struct virtio_snd_pcm_desc *)cfg->data;
    pcm_desc->length = sizeof(struct virtio_snd_pcm_desc);
    pcm_desc->type = VIRTIO_SND_DESC_PCM;

    length = pcm_desc->length;

    pcm_stream_desc = (struct virtio_snd_pcm_stream_desc *)(pcm_desc + 1);

    for (i = VIRTIO_SND_PCM_T_PLAYBACK; i <= VIRTIO_SND_PCM_T_CAPTURE; i++) {
        VirtIOSoundPCMStream *stream = snd->pcm->streams[i];
        uint16_t channels_min = VIOSND_PCM_MIN_CHANNELS;
        uint16_t channels_max = VIOSND_PCM_MAX_CHANNELS;
        uint32_t formats = VIOSND_PCM_FORMATS;
        uint32_t rates = VIOSND_PCM_RATES;

        if (!stream) {
            continue;
        }

        pcm_desc->nstreams++;

        pcm_stream_desc->length = sizeof(struct virtio_snd_pcm_stream_desc);
        pcm_stream_desc->type = VIRTIO_SND_DESC_PCM_STREAM;
        pcm_stream_desc->stream_type = i;
        virtio_stw_p(vdev, &pcm_stream_desc->channels_min, channels_min);
        virtio_stw_p(vdev, &pcm_stream_desc->channels_max, channels_max);
        virtio_stl_p(vdev, &pcm_stream_desc->formats, formats);
        virtio_stl_p(vdev, &pcm_stream_desc->rates, rates);

        length += pcm_stream_desc->length;
        pcm_stream_desc++;
    }

    virtio_stl_p(vdev, &cfg->length, length);

    return VIRTIO_SND_S_OK;
}

static uint32_t viosnd_pcm_set_format(VirtIOSound *snd,
                                      struct virtio_snd_hdr *request,
                                      size_t req_size,
                                      struct virtio_snd_hdr *response,
                                      size_t resp_size)
{
    VirtIODevice *vdev = &snd->parent_obj;
    VirtIOSoundPCMStream *stream;
    struct audsettings as;
    struct virtio_snd_pcm_set_format *msg =
        (struct virtio_snd_pcm_set_format *)request;

    if ((req_size != sizeof(struct virtio_snd_pcm_set_format)) ||
        (resp_size != sizeof(struct virtio_snd_hdr))) {
        return VIRTIO_SND_S_EINVALID;
    }

    stream = viosnd_pcm_stream(snd, (struct virtio_snd_pcm_hdr *)request);
    if (!stream) {
        return VIRTIO_SND_S_EINVALID;
    }

    as.nchannels = virtio_lduw_p(vdev, &msg->channels);
    as.fmt = viosnd_pcm_qemu_format(virtio_lduw_p(vdev, &msg->format));
    as.freq = viosnd_pcm_qemu_rate(virtio_lduw_p(vdev, &msg->rate));

    if (as.nchannels < VIOSND_PCM_MIN_CHANNELS ||
        as.nchannels > VIOSND_PCM_MAX_CHANNELS ||
        as.fmt == -1 || as.freq == -1) {
        AUD_log(AUDIO_CAP, "PCM/SET_FORMAT - wrong parameters\n");
        return VIRTIO_SND_S_EINVALID;
    }

    stream->desired_as.nchannels = as.nchannels;
    stream->desired_as.fmt = as.fmt;
    stream->desired_as.freq = as.freq;

    return VIRTIO_SND_S_OK;
}

static uint32_t viosnd_pcm_prepare(VirtIOSound *snd,
                                   struct virtio_snd_hdr *request,
                                   size_t req_size,
                                   struct virtio_snd_hdr *response,
                                   size_t resp_size)
{
    VirtIOSoundPCMStream *stream;

    if ((req_size != sizeof(struct virtio_snd_pcm_hdr)) ||
        (resp_size != sizeof(struct virtio_snd_hdr))) {
        return VIRTIO_SND_S_EINVALID;
    }

    stream = viosnd_pcm_stream(snd, (struct virtio_snd_pcm_hdr *)request);
    if (!stream) {
        return VIRTIO_SND_S_EINVALID;
    }

    viosnd_pcm_flush(stream);

    if (stream->as.nchannels != stream->desired_as.nchannels ||
        stream->as.fmt != stream->desired_as.fmt ||
        stream->as.freq != stream->desired_as.freq) {
        stream->as = stream->desired_as;

        viosnd_pcm_open(stream);
    }

    return VIRTIO_SND_S_OK;
}

static uint32_t viosnd_pcm_start(VirtIOSound *snd,
                                 struct virtio_snd_hdr *request,
                                 size_t req_size,
                                 struct virtio_snd_hdr *response,
                                 size_t resp_size)
{
    VirtIOSoundPCMStream *stream;

    if ((req_size != sizeof(struct virtio_snd_pcm_hdr)) ||
        (resp_size != sizeof(struct virtio_snd_hdr))) {
        return VIRTIO_SND_S_EINVALID;
    }

    stream = viosnd_pcm_stream(snd, (struct virtio_snd_pcm_hdr *)request);
    if (!stream) {
        return VIRTIO_SND_S_EINVALID;
    }

    viosnd_pcm_set_active(stream, 1);

    return VIRTIO_SND_S_OK;
}

static uint32_t viosnd_pcm_stop(VirtIOSound *snd,
                                struct virtio_snd_hdr *request,
                                size_t req_size,
                                struct virtio_snd_hdr *response,
                                size_t resp_size)
{
    VirtIOSoundPCMStream *stream;

    if ((req_size != sizeof(struct virtio_snd_pcm_hdr)) ||
        (resp_size != sizeof(struct virtio_snd_hdr))) {
        return VIRTIO_SND_S_EINVALID;
    }

    stream = viosnd_pcm_stream(snd, (struct virtio_snd_pcm_hdr *)request);
    if (!stream) {
        return VIRTIO_SND_S_EINVALID;
    }

    viosnd_pcm_set_active(stream, 0);

    return VIRTIO_SND_S_OK;
}

typedef uint32_t (*dispatch_cb_t)(VirtIOSound *snd,
                                 struct virtio_snd_hdr *request,
                                 size_t req_size,
                                 struct virtio_snd_hdr *response,
                                 size_t resp_size);

typedef struct {
    uint32_t code;
    uint32_t (*handle)(VirtIOSound *snd, struct virtio_snd_hdr *request,
                       size_t req_size, struct virtio_snd_hdr *response,
                       size_t resp_size);
} VirtIOSoundCtlHandler;

static VirtIOSoundCtlHandler g_handlers[] = {
    { VIRTIO_SND_R_BASE_GET_CFG, viosnd_base_configuration },
    { VIRTIO_SND_R_PCM_SET_FORMAT, viosnd_pcm_set_format },
    { VIRTIO_SND_R_PCM_PREPARE, viosnd_pcm_prepare },
    { VIRTIO_SND_R_PCM_START, viosnd_pcm_start },
    { VIRTIO_SND_R_PCM_STOP, viosnd_pcm_stop },
    { VIRTIO_SND_R_PCM_PAUSE, viosnd_pcm_stop },
    { VIRTIO_SND_R_PCM_UNPAUSE, viosnd_pcm_start },
    { 0 }
};

static void viosnd_ctl_kick_cb(VirtIODevice *vdev, VirtQueue *vqueue)
{
    VirtIOSound *snd = VIRTIO_SND(vdev);

    for (;;) {
        VirtQueueElement *element;
        uint32_t status = VIRTIO_SND_S_EINVALID;
        struct virtio_snd_hdr *request = NULL;
        size_t req_size;
        struct virtio_snd_hdr *response = NULL;
        size_t resp_size;
        uint32_t request_code;
        uint32_t i;

        element = virtqueue_pop(vqueue, sizeof(VirtQueueElement));
        if (!element) {
            break;
        }

        resp_size = iov_size(element->in_sg, element->in_num);
        if (resp_size < sizeof(struct virtio_snd_hdr) ||
            resp_size > VIOSND_RESPONSE_MAX_SIZE) {
            virtio_error(vdev, "Wrong response size (%lu bytes)\n", resp_size);
            goto on_failure;
        }

        response = g_malloc0(resp_size);

        req_size = iov_size(element->out_sg, element->out_num);
        if (req_size < sizeof(struct virtio_snd_hdr) ||
            req_size > VIOSND_REQUEST_MAX_SIZE) {
            virtio_error(vdev, "Wrong request size (%lu bytes)\n", req_size);
            goto on_failure;
        }

        request = g_malloc0(req_size);

        iov_to_buf(element->out_sg, element->out_num, 0, request, req_size);

        request_code = virtio_ldl_p(&snd->parent_obj, &request->code);

        for (i = 0; g_handlers[i].handle; ++i) {
            if (request_code == g_handlers[i].code) {
                status = g_handlers[i].handle(snd, request, req_size, response,
                                              resp_size);
                break;
            }
        }

on_failure:
        if (response) {
            virtio_stl_p(vdev, &response->code, status);

            iov_from_buf(element->in_sg, element->in_num, 0, response,
                         resp_size);
        }

        virtqueue_push(vqueue, element, resp_size);

        g_free(element);
        g_free(request);
        g_free(response);
    }

    virtio_notify(vdev, vqueue);
}

static void viosnd_pcm_kick_cb(VirtIODevice *vdev, VirtQueue *vqueue)
{
    VirtIOSound *snd = VIRTIO_SND(vdev);

    for (;;) {
        VirtIOSoundPCMStream *stream;
        VirtIOSoundPCMBlock *block;
        VirtQueueElement *element;
        size_t available;
        uint8_t stream_type;
        uint32_t status = VIRTIO_SND_S_OK;
        size_t status_offset = 0;
        uint32_t length = 0;

        element = virtqueue_pop(vqueue, sizeof(VirtQueueElement));
        if (!element) {
            break;
        }

        iov_to_buf(element->out_sg, element->out_num, 0, &stream_type,
                   sizeof(stream_type));

        switch (stream_type) {
        case VIRTIO_SND_PCM_T_PLAYBACK:
        {
            stream = snd->pcm->streams[VIRTIO_SND_PCM_T_PLAYBACK];
            available = iov_size(element->out_sg, element->out_num) - sizeof(stream_type);

            length = available;

            block = g_malloc(sizeof(VirtIOSoundPCMBlock) + available);
            block->size = available;
            block->offset = 0;

            iov_to_buf(element->out_sg, element->out_num, sizeof(stream_type),
                       block->data, available);

            qemu_mutex_lock(&stream->queue_mutex);
            QSIMPLEQ_INSERT_TAIL(&stream->queue, block, entry);
            qemu_mutex_unlock(&stream->queue_mutex);

            break;
        }
        case VIRTIO_SND_PCM_T_CAPTURE:
        {
            VirtIOSoundPCMBlock *next;

            stream = snd->pcm->streams[VIRTIO_SND_PCM_T_CAPTURE];
            available = iov_size(element->in_sg, element->in_num) - sizeof(status);

            status_offset = available;

            qemu_mutex_lock(&stream->queue_mutex);
            QSIMPLEQ_FOREACH_SAFE(block, &stream->queue, entry, next) {
                int size = MIN(block->size, available);

                iov_from_buf(element->in_sg, element->in_num, length,
                             block->data + block->offset, size);

                length += size;

                block->size -= size;
                if (!block->size) {
                    QSIMPLEQ_REMOVE(&stream->queue, block, VirtIOSoundPCMBlock,
                                    entry);
                    g_free(block);
                } else {
                    block->offset += size;
                }

                available -= size;
                if (!available) {
                    break;
                }
            }
            qemu_mutex_unlock(&stream->queue_mutex);

            break;
        }
        default:
        {
            AUD_log(AUDIO_CAP, "Wrong pcm xfer stream type (%u)\n",
                    stream_type);

            status = VIRTIO_SND_S_EINVALID;

            break;
        }
        }

        virtio_stl_p(vdev, &status, status);

        iov_from_buf(element->in_sg, element->in_num, status_offset, &status,
                     sizeof(status));

        virtqueue_push(vqueue, element, length);
        g_free(element);
    }

    virtio_notify(vdev, vqueue);
}

static void viosnd_set_status(VirtIODevice *vdev, uint8_t status)
{
    VirtIOSound *snd = VIRTIO_SND(vdev);
    int i;

    for (i = VIRTIO_SND_PCM_T_PLAYBACK; i <= VIRTIO_SND_PCM_T_CAPTURE; i++) {
        VirtIOSoundPCMStream *stream = snd->pcm->streams[i];

        if (status & VIRTIO_CONFIG_S_DRIVER_OK) {
            stream->as.nchannels = VIOSND_PCM_MIN_CHANNELS;
            stream->as.fmt = AUDIO_FORMAT_S16;
            stream->as.freq = 44100;
            stream->as.endianness = virtio_access_is_big_endian(vdev);

            stream->desired_as = stream->as;

            viosnd_pcm_open(stream);
        } else {
            viosnd_pcm_close(stream);
        }
    }
}

static uint64_t viosnd_get_features(VirtIODevice *vdev, uint64_t features,
                                    Error **errp)
{
    return features;
}

static void viosnd_get_config(VirtIODevice *vdev, uint8_t *data)
{
    VirtIOSound *snd = VIRTIO_SND(vdev);
    struct virtio_snd_config *config = (struct virtio_snd_config *)data;
    struct virtio_snd_queue_info* info;

    config->nqueues = snd->nqueues;

    info = (struct virtio_snd_queue_info*)(config + 1);
    info->function = VIRTIO_SND_FN_BASE;
    info->device_id = 0;
    info->subdevice_id = 0;

    info++;
    info->function = VIRTIO_SND_FN_PCM;
    info->device_id = 0;
    info->subdevice_id = 0;
}

static int viosnd_set_pcm(VirtIOSound *snd)
{
    VirtIOSoundPCM *pcm;
    int i;

    pcm = g_new0(VirtIOSoundPCM, 1);
    pcm->snd = snd;

    for (i = VIRTIO_SND_PCM_T_PLAYBACK; i <= VIRTIO_SND_PCM_T_CAPTURE; i++) {
        pcm->streams[i] = g_new0(VirtIOSoundPCMStream, 1);
        pcm->streams[i]->pcm = pcm;
        qemu_mutex_init(&pcm->streams[i]->queue_mutex);
        QSIMPLEQ_INIT(&pcm->streams[i]->queue);
    }

    snd->pcm = pcm;

    return 0;
}

static void viosnd_device_realize(DeviceState *dev, Error **errp)
{
    VirtIODevice *vdev = VIRTIO_DEVICE(dev);
    VirtIOSound *snd = VIRTIO_SND(vdev);
    size_t config_size = sizeof(struct virtio_snd_config);

    AUD_register_card ("virtio-snd", &snd->card);

    if (viosnd_set_pcm(snd)) {
        error_setg(errp, "No PCM streams created");
        return;
    }

    snd->nqueues = 2;
    config_size += snd->nqueues * sizeof(struct virtio_snd_queue_info);

    virtio_init(vdev, "virtio-snd", VIRTIO_ID_SOUND, config_size);

    snd->queues = g_new0(VirtQueue*, snd->nqueues);
    snd->queues[0] = virtio_add_queue(vdev, 64, viosnd_ctl_kick_cb);
    snd->queues[1] = virtio_add_queue(vdev, 128, viosnd_pcm_kick_cb);
}

static void viosnd_device_unrealize(DeviceState *dev, Error **errp)
{
    VirtIOSound *snd = VIRTIO_SND(dev);
    int i;

    for (i = VIRTIO_SND_PCM_T_PLAYBACK; i <= VIRTIO_SND_PCM_T_CAPTURE; i++) {
        VirtIOSoundPCMStream *stream = snd->pcm->streams[i];

        viosnd_pcm_close(stream);
        viosnd_pcm_flush(stream);

        g_free(stream);
    }

    AUD_remove_card(&snd->card);

    g_free(snd->pcm);
    g_free(snd->queues);

    virtio_cleanup(VIRTIO_DEVICE(dev));
}

static void viosnd_class_init(ObjectClass *klass, void *data)
{
    DeviceClass *dc = DEVICE_CLASS(klass);
    VirtioDeviceClass *vdc = VIRTIO_DEVICE_CLASS(klass);

    set_bit(DEVICE_CATEGORY_SOUND, dc->categories);
    vdc->realize = viosnd_device_realize;
    vdc->unrealize = viosnd_device_unrealize;
    vdc->get_features = viosnd_get_features;
    vdc->get_config = viosnd_get_config;
    vdc->set_status = viosnd_set_status;
}

static const TypeInfo viosnd_info = {
    .name = TYPE_VIRTIO_SND,
    .parent = TYPE_VIRTIO_DEVICE,
    .instance_size = sizeof(VirtIOSound),
    .class_init = viosnd_class_init,
};

static void virtio_register_types(void)
{
    type_register_static(&viosnd_info);
}

type_init(virtio_register_types)
