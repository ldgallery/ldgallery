<!--
-- ldgallery - A static generator which turns a collection of tagged
--             pictures into a searchable web gallery.
--
-- Copyright (C) 2020  Pacien TRAN-GIRARD
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
-->

<template>
  <div class="container-vh-centering">
    <a
      :class="$style.content"
      :download="itemFileName"
      :href="itemResourceUrl"
    >
      <!-- TODO: show thumbnail instead of this generic file download icon? -->
      <fa-icon
        :class="$style.icon"
        :icon="faFileDownload"
        size="6x"
      />
      <div>{{ t("download.download-file-fmt", [itemFileName]) }}</div>
    </a>
  </div>
</template>

<script setup lang="ts">
import { DownloadableItem } from '@/@types/gallery';
import { useNavigation } from '@/services/navigation';
import { useItemResource } from '@/services/ui/ldItemResourceUrl';
import { faFileDownload } from '@fortawesome/free-solid-svg-icons';
import { computed, PropType } from 'vue';
import { useI18n } from 'vue-i18n';

const props = defineProps({
  item: { type: Object as PropType<DownloadableItem>, required: true },
});

const { t } = useI18n();
const navigation = useNavigation();

const itemFileName = computed(() => navigation.getFileName(props.item));
const { itemResourceUrl } = useItemResource(props.item);
</script>

<style lang="scss" module>
.content {
  text-align: center;
}

.icon {
  margin-bottom: 0.15em;
}
</style>
