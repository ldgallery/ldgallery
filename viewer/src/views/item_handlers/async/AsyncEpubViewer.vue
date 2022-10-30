<!--
-- ldgallery - A static generator which turns a collection of tagged
--             pictures into a searchable web gallery.
--
-- Copyright (C) 2022  Pacien TRAN-GIRARD
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
  <div :class="$style.container">
    <div
      ref="view"
      :class="$style.epubView"
      class="scrollbar"
    />

    <ul
      v-if="prevSection || nextSection"
      :class="$style.navBar"
    >
      <li>
        <a
          v-if="prevSection"
          @click.prevent="goToPrevSection"
        >« {{ prevSectionLabel }}</a>
      </li>

      <li>
        {{ currSectionLabel }}
      </li>

      <li>
        <a
          v-if="nextSection"
          @click.prevent="goToNextSection"
        >{{ nextSectionLabel }} »</a>
      </li>
    </ul>
  </div>
</template>

<script setup lang="ts">

import { EPUBItem } from '@/@types/gallery';
import { useItemResource } from '@/services/ui/ldItemResourceUrl';
import ePub from 'epubjs';
import { SpineItem } from 'epubjs/types/section';
import { PropType, Ref, ref, toRef, computed, watchEffect } from 'vue';
import { useI18n } from 'vue-i18n';
const { t } = useI18n();

const props = defineProps({
  item: { type: Object as PropType<EPUBItem>, required: true },
});

const { itemResourceUrl } = useItemResource(toRef(props, 'item'));
const view = ref<HTMLDivElement>();

const book = computed(() => ePub(itemResourceUrl.value));

const rendition = computed(() => {
  if (!view.value) return;
  return book.value.renderTo(view.value, {
    flow: 'scrolled-doc',
    width: '100%',
    fullsize: true,
  });
});

const currSection = ref<SpineItem>();
const prevSection = ref<SpineItem>();
const nextSection = ref<SpineItem>();

// TODO: reflow on side panel open/close event, like when resizing the window

watchEffect(async() => {
  if (!rendition.value) return;
  await rendition.value.display();
  rendition.value.on('rendered', updateNavigation);
});

function updateNavigation(currentSection: SpineItem) {
  currSection.value = currentSection;
  prevSection.value = currentSection.prev();
  nextSection.value = currentSection.next();
}

const currSectionLabel = computed(() => getSectionTitle(currSection) ?? '');
const prevSectionLabel = computed(() =>
  getSectionTitle(prevSection) ?? t('epubViewer.previousSection'));
const nextSectionLabel = computed(() =>
  getSectionTitle(nextSection) ?? t('epubViewer.nextSection'));

function getSectionTitle(section: Ref<SpineItem | undefined>): string | null {
  if (!section.value?.href) return null;
  return book.value?.navigation.get(section.value.href).label;
}

function goToPrevSection() {
  rendition.value?.prev();
}

function goToNextSection() {
  rendition.value?.next();
}
</script>

<style lang="scss" module>
@import "~@/assets/scss/theme";

.container {
  display: flex;
  flex-direction: column;
  height: 100%;
}

.epubView {
  flex: 1;
  overflow-x: hidden;
}

.navBar {
  display: flex;
  flex-direction: row;
  list-style-type: none;
  margin: 0;
  padding: .75em;

  background-color: $panel-bottom-bgcolor;
  color: $panel-bottom-txtcolor;

  > * {
    flex: 1;
    text-align: center;
  }
}
</style>
