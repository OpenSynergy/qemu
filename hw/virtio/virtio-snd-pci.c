/*
 * Virtio Sound Device PCI Bindings
 *
 * Copyright (C) 2019 OpenSynergy GmbH
 *
 * This work is licensed under the terms of the GNU GPL, version 2 or
 * (at your option) any later version.  See the COPYING file in the
 * top-level directory.
 */

#include <linux/types.h>

#include "qemu/osdep.h"
#include "hw/virtio/virtio-pci.h"
#include "hw/virtio/virtio-snd.h"

typedef struct VirtIOSoundPCI VirtIOSoundPCI;

/*
 * virtio-snd-pci: This extends VirtioPCIProxy.
 */
#define TYPE_VIRTIO_SND_PCI "virtio-snd-pci-base"
#define VIRTIO_SND_PCI(obj) \
        OBJECT_CHECK(VirtIOSoundPCI, (obj), TYPE_VIRTIO_SND_PCI)

struct VirtIOSoundPCI {
    VirtIOPCIProxy parent;
    VirtIOSound vdev;
};

static Property virtio_snd_pci_properties[] = {
    DEFINE_PROP_UINT32("class", VirtIOPCIProxy, class_code, 0),
    DEFINE_PROP_END_OF_LIST(),
};

static void virtio_snd_pci_realize(VirtIOPCIProxy *vpci_dev, Error **errp)
{
    VirtIOSoundPCI *vsnd = VIRTIO_SND_PCI(vpci_dev);
    DeviceState *vdev = DEVICE(&vsnd->vdev);

    qdev_set_parent_bus(vdev, BUS(&vpci_dev->bus));
    object_property_set_bool(OBJECT(vdev), true, "realized", errp);
}

static void virtio_snd_pci_class_init(ObjectClass *klass, void *data)
{
    DeviceClass *dc = DEVICE_CLASS(klass);
    VirtioPCIClass *k = VIRTIO_PCI_CLASS(klass);
    PCIDeviceClass *pcidev_k = PCI_DEVICE_CLASS(klass);

    k->realize = virtio_snd_pci_realize;
    set_bit(DEVICE_CATEGORY_SOUND, dc->categories);
    dc->props = virtio_snd_pci_properties;
    pcidev_k->vendor_id = PCI_VENDOR_ID_REDHAT_QUMRANET;
    pcidev_k->device_id = PCI_DEVICE_ID_VIRTIO_SND;
    pcidev_k->revision = VIRTIO_PCI_ABI_VERSION;
    pcidev_k->class_id = PCI_CLASS_MULTIMEDIA_AUDIO;
}

static void virtio_snd_pci_instance_init(Object *obj)
{
    VirtIOSoundPCI *vsnd = VIRTIO_SND_PCI(obj);

    virtio_instance_init_common(obj, &vsnd->vdev, sizeof(vsnd->vdev),
                                TYPE_VIRTIO_SND);
}

static const VirtioPCIDeviceTypeInfo virtio_snd_pci_info = {
    .base_name             = TYPE_VIRTIO_SND_PCI,
    .generic_name          = "virtio-snd-pci",
    .transitional_name     = "virtio-snd-pci-transitional",
    .non_transitional_name = "virtio-snd-pci-non-transitional",
    .instance_size = sizeof(VirtIOSoundPCI),
    .instance_init = virtio_snd_pci_instance_init,
    .class_init    = virtio_snd_pci_class_init,
};

static void virtio_snd_pci_register(void)
{
    virtio_pci_types_register(&virtio_snd_pci_info);
}

type_init(virtio_snd_pci_register)
