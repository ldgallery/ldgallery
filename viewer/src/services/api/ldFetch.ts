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

import { MaybeComputedRef, useFetch } from '@vueuse/core';
import { createToast } from 'mosha-vue-toastify';

export const useLdFetch = (url: MaybeComputedRef<string>) => {
  const fetchReturn = useFetch(url, { refetch: true });
  fetchReturn.onFetchError((error) => {
    createToast(String(error), {
      type: 'danger',
      position: 'top-center',
      timeout: 10000,
      showIcon: true,
      onClose: fetchReturn.execute,
    });
  });
  return fetchReturn;
};
