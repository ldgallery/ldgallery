/* ldgallery - A static generator which turns a collection of tagged
--             pictures into a searchable web gallery.
--
-- Copyright (C) 2019-2022  Guillaume FOUET
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

import { useGalleryStore } from '@/store/galleryStore';
import { useTitle } from '@vueuse/core';
import { computed } from 'vue';

export const useLdTitle = () => {
  const galleryStore = useGalleryStore();

  const title = computed(() => {
    const { currentItem, galleryTitle } = galleryStore;
    return currentItem?.title
      ? `${currentItem.title} • ${galleryTitle}`
      : galleryTitle;
  });
  useTitle(title);
};
