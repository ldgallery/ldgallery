import { Item } from '@/@types/gallery';
import { useGalleryStore } from '@/store/galleryStore';
import { computed, ToRef } from 'vue';
import { isDownloadableItem } from '../itemGuards';

export const useItemResource = (item: ToRef<Item>) => {
  const galleryStore = useGalleryStore();
  const itemResourceUrl = computed(() => isDownloadableItem(item.value) ? galleryStore.resourceRoot + item.value.properties.resource : '');
  const thumbnailResourceUrl = computed(() => item.value.thumbnail ? galleryStore.resourceRoot + item.value.thumbnail.resource : '');

  return {
    itemResourceUrl,
    thumbnailResourceUrl,
  };
};
