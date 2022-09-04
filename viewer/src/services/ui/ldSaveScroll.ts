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

import { MaybeElementRef, unrefElement } from '@vueuse/core';
import { nextTick, watch } from 'vue';
import { useRoute } from 'vue-router';

type ScrollPosition = Record<string, number>;

export const useLdSaveScroll = (element: MaybeElementRef) => {
  const scrollPositions: ScrollPosition = {};
  const route = useRoute();

  watch(() => decodeURIComponent(route.path), (newRoute, oldRoute) => {
    const el = unrefElement(element);
    if (!el) return;

    scrollPositions[oldRoute] = el.scrollTop / el.scrollHeight;
    nextTick(() => (el.scrollTop = scrollPositions[newRoute] * el.scrollHeight));
  });
};
