import { DirectoryItem, DownloadableItem, Item } from '@/@types/gallery';
import { ItemType } from '@/@types/itemType';

export function isDirectory(item: Item | null): item is DirectoryItem {
  return item?.properties.type === ItemType.DIRECTORY;
}

export function isDownloadableItem(item: Item | null): item is DownloadableItem {
  if (!item?.properties) return false;
  return 'resource' in item.properties;
}
