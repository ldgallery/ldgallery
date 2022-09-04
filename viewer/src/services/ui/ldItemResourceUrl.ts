import { Item } from '@/@types/gallery';
import { useGalleryStore } from '@/store/galleryStore';
import { computed } from 'vue';
import { isDownloadableItem } from '../itemGuards';

export const useItemResource = (item: Item) => {
  const galleryStore = useGalleryStore();
  const itemResourceUrl = computed(() => isDownloadableItem(item) ? galleryStore.resourceRoot + item.properties.resource : '');
  const thumbnailResourceUrl = computed(() => item.thumbnail ? galleryStore.resourceRoot + item.thumbnail.resource : '');

  return {
    itemResourceUrl,
    thumbnailResourceUrl,
  };
};
