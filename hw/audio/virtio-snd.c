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
    uint32_t type = le32toh(request->stream);

    switch (type) {
    case VIRTIO_SND_PCM_T_OUTPUT:
    case VIRTIO_SND_PCM_T_INPUT:
        return snd->pcm->streams[type];
    }

    return NULL;
}

static inline bool viosnd_pcm_is_playback(VirtIOSoundPCMStream *stream)
{
    return stream->pcm->streams[VIRTIO_SND_PCM_T_OUTPUT] == stream;
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

static uint32_t
viosnd_pcm_chmap_info(struct VirtIOSound *snd,
                      struct virtio_snd_hdr *request,
                      size_t req_size,
                      struct virtio_snd_hdr *response,
                      size_t resp_size)
{
    (void)req_size;
    (void)response;
    (void)resp_size;

    if (!viosnd_pcm_stream(snd, (struct virtio_snd_pcm_hdr *)request)) {
        return VIRTIO_SND_S_BAD_MSG;
    }

    return VIRTIO_SND_S_NOT_SUPP;
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
        return VIRTIO_SND_S_BAD_MSG;
    }

    stream = viosnd_pcm_stream(snd, (struct virtio_snd_pcm_hdr *)request);
    if (!stream) {
        return VIRTIO_SND_S_BAD_MSG;
    }

    as.nchannels = virtio_lduw_p(vdev, &msg->channels);
    as.fmt = viosnd_pcm_qemu_format(virtio_lduw_p(vdev, &msg->format));
    as.freq = viosnd_pcm_qemu_rate(virtio_lduw_p(vdev, &msg->rate));

    if (as.nchannels < VIOSND_PCM_MIN_CHANNELS ||
        as.nchannels > VIOSND_PCM_MAX_CHANNELS ||
        as.fmt == -1 || as.freq == -1) {
        AUD_log(AUDIO_CAP, "PCM/SET_FORMAT - wrong parameters\n");
        return VIRTIO_SND_S_BAD_MSG;
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
        return VIRTIO_SND_S_BAD_MSG;
    }

    stream = viosnd_pcm_stream(snd, (struct virtio_snd_pcm_hdr *)request);
    if (!stream) {
        return VIRTIO_SND_S_BAD_MSG;
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
        return VIRTIO_SND_S_BAD_MSG;
    }

    stream = viosnd_pcm_stream(snd, (struct virtio_snd_pcm_hdr *)request);
    if (!stream) {
        return VIRTIO_SND_S_BAD_MSG;
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
        return VIRTIO_SND_S_BAD_MSG;
    }

    stream = viosnd_pcm_stream(snd, (struct virtio_snd_pcm_hdr *)request);
    if (!stream) {
        return VIRTIO_SND_S_BAD_MSG;
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
    { VIRTIO_SND_R_PCM_CHMAP_INFO, viosnd_pcm_chmap_info },
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
        uint32_t status = VIRTIO_SND_S_BAD_MSG;
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

static uint32_t viosnd_pcm_write(VirtIOSoundPCMStream *stream,
                                 VirtQueueElement *element)
{
    VirtIOSoundPCMBlock *fragment;
    size_t size = iov_size(element->out_sg, element->out_num) -
                  sizeof(struct virtio_snd_pcm_xfer);

    fragment = g_malloc(sizeof(VirtIOSoundPCMBlock) + size);
    fragment->size = size;
    fragment->offset = 0;

    iov_to_buf(element->out_sg, element->out_num,
               sizeof(struct virtio_snd_pcm_xfer), fragment->data, size);

    qemu_mutex_lock(&stream->queue_mutex);
    QSIMPLEQ_INSERT_TAIL(&stream->queue, fragment, entry);
    qemu_mutex_unlock(&stream->queue_mutex);

    return fragment->size;
}

static uint32_t viosnd_pcm_read(VirtIOSoundPCMStream *stream,
                                VirtQueueElement *element)
{
    size_t available = iov_size(element->in_sg, element->in_num) -
                                sizeof(struct virtio_snd_pcm_xfer_status);
    size_t nbytes = 0;
    VirtIOSoundPCMBlock *block, *next;

    qemu_mutex_lock(&stream->queue_mutex);
    QSIMPLEQ_FOREACH_SAFE(block, &stream->queue, entry, next) {
        int size = MIN(block->size, available);

        iov_from_buf(element->in_sg, element->in_num, nbytes,
                     block->data + block->offset, size);

        nbytes += size;

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

    return nbytes;
}

static void viosnd_pcm_kick_cb(VirtIODevice *vdev, VirtQueue *vqueue)
{
    VirtIOSound *snd = VIRTIO_SND(vdev);

    for (;;) {
        VirtQueueElement *element;
        struct virtio_snd_pcm_xfer hdr = { 0 };
        size_t hdr_size = sizeof(hdr);
        struct virtio_snd_pcm_xfer_status status = { 0 };
        uint32_t stream_type;
        size_t status_offset = 0;
        uint32_t nbytes = 0;

        element = virtqueue_pop(vqueue, sizeof(VirtQueueElement));
        if (!element) {
            break;
        }

        iov_to_buf(element->out_sg, element->out_num, 0, &hdr, hdr_size);

        stream_type = virtio_ldl_p(&snd->parent_obj, &hdr.stream);

        switch (stream_type) {
        case VIRTIO_SND_PCM_T_OUTPUT:
            nbytes = viosnd_pcm_write(
                snd->pcm->streams[VIRTIO_SND_PCM_T_OUTPUT], element);

            break;
        case VIRTIO_SND_PCM_T_INPUT:
            status_offset = iov_size(element->in_sg, element->in_num) -
                            sizeof(status);

            nbytes = viosnd_pcm_read(snd->pcm->streams[VIRTIO_SND_PCM_T_INPUT],
                                     element);

            break;
        }

        virtio_stl_p(vdev, &status.status, VIRTIO_SND_S_OK);
        virtio_stl_p(vdev, &status.actual_length, nbytes);

        iov_from_buf(element->in_sg, element->in_num, status_offset, &status,
                     sizeof(status));

        virtqueue_push(vqueue, element, nbytes);
        g_free(element);
    }

    virtio_notify(vdev, vqueue);
}

static void viosnd_set_status(VirtIODevice *vdev, uint8_t status)
{
    VirtIOSound *snd = VIRTIO_SND(vdev);
    int i;

    for (i = VIRTIO_SND_PCM_T_OUTPUT; i <= VIRTIO_SND_PCM_T_INPUT; i++) {
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

static int viosnd_set_pcm(VirtIOSound *snd)
{
    VirtIOSoundPCM *pcm;
    int i;

    pcm = g_new0(VirtIOSoundPCM, 1);
    pcm->snd = snd;

    for (i = VIRTIO_SND_PCM_T_OUTPUT; i <= VIRTIO_SND_PCM_T_INPUT; i++) {
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

    AUD_register_card ("virtio-snd", &snd->card);

    if (viosnd_set_pcm(snd)) {
        error_setg(errp, "No PCM streams created");
        return;
    }

    virtio_init(vdev, "virtio-snd", VIRTIO_ID_SOUND,
                sizeof(struct virtio_snd_config));
    snd->queues[VIOSND_QUEUE_CTL] = virtio_add_queue(vdev, 64,
                                                     viosnd_ctl_kick_cb);
    snd->queues[VIOSND_QUEUE_PCM] = virtio_add_queue(vdev, 128,
                                                     viosnd_pcm_kick_cb);
}

static void viosnd_device_unrealize(DeviceState *dev, Error **errp)
{
    VirtIOSound *snd = VIRTIO_SND(dev);
    int i;

    for (i = VIRTIO_SND_PCM_T_OUTPUT; i <= VIRTIO_SND_PCM_T_INPUT; i++) {
        VirtIOSoundPCMStream *stream = snd->pcm->streams[i];

        viosnd_pcm_close(stream);
        viosnd_pcm_flush(stream);

        g_free(stream);
    }

    AUD_remove_card(&snd->card);

    g_free(snd->pcm);

    virtio_cleanup(VIRTIO_DEVICE(dev));
}

static uint64_t viosnd_get_features(VirtIODevice *vdev, uint64_t features,
                                    Error **errp)
{
    return features | 1UL << VIRTIO_SND_F_PCM_OUTPUT |
           1UL << VIRTIO_SND_F_PCM_INPUT | 1UL << VIRTIO_F_VERSION_1;
}

static void viosnd_get_config(VirtIODevice *vdev, uint8_t *data)
{
    struct virtio_snd_config *config = (struct virtio_snd_config *)data;

    config->pcm.output.channels_min = VIOSND_PCM_MIN_CHANNELS;
    config->pcm.output.channels_max = VIOSND_PCM_MAX_CHANNELS;
    virtio_stw_p(vdev, &config->pcm.output.formats, VIOSND_PCM_FORMATS);
    virtio_stw_p(vdev, &config->pcm.output.rates, VIOSND_PCM_RATES);

    config->pcm.input = config->pcm.output;
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
