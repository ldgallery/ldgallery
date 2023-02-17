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
  <div class="flex-column container-vh-centering">
    <ItemThumbnail
      :item="item"
      :class="$style.audiothumb"
    />
    <audio
      :class="$style.player"
      :src="itemResourceUrl"
      preload="auto"
      controls
    >
      <a
        :download="itemFileName"
        :href="itemResourceUrl"
        v-text="t('download.download-file-fmt', [itemFileName])"
      />
    </audio>
  </div>
</template>

<script setup lang="ts">
import { AudioItem } from '@/@types/gallery';
import { useNavigation } from '@/services/navigation';
import { useItemResource } from '@/services/ui/ldItemResourceUrl';
import { computed, PropType, toRef } from 'vue';
import { useI18n } from 'vue-i18n';
import ItemThumbnail from '../ItemThumbnail.vue';

const props = defineProps({
  item: { type: Object as PropType<AudioItem>, required: true },
});

const { t } = useI18n();
const navigation = useNavigation();

const { itemResourceUrl } = useItemResource(toRef(props, 'item'));
const itemFileName = computed(() => navigation.getFileName(props.item));
</script>

<style lang="scss" module>
@import "~@/assets/scss/theme";

.audiothumb {
  color: $text-light;
}

.player {
  width: 100%;
  max-width: 500px;
  margin-top: 1em;
  text-align: center; // for fallback download link
}
</style>
