<!-- ldgallery - A static generator which turns a collection of tagged
--             pictures into a searchable web gallery.
--
-- Copyright (C) 2019-2022  Guillaume FOUET
--               2020       Pacien TRAN-GIRARD
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
  <div :class="$style.proposition">
    <h2
      v-if="showCategory && Object.keys(propositions).length"
      :class="[$style.subtitle, $style.category]"
    >
      {{ title }}
    </h2>
    <div
      v-for="proposed in proposedTags"
      :key="proposed.rawTag"
    >
      <LdLink
        :class="$style.operationBtns"
        :title="t('tag-propositions.substraction')"
        @click="add(Operation.SUBSTRACTION, proposed.rawTag)"
      >
        <fa-icon
          :icon="faMinus"
          alt="[-]"
        />
      </LdLink>

      <LdLink
        :class="$style.operationBtns"
        :title="t('tag-propositions.addition')"
        @click="add(Operation.ADDITION, proposed.rawTag)"
      >
        <fa-icon
          :icon="faPlus"
          alt="[+]"
        />
      </LdLink>

      <LdLink
        :class="$style.operationTag"
        :title="t('tag-propositions.intersection')"
        @click="add(Operation.INTERSECTION, proposed.rawTag)"
      >
        {{ proposed.rawTag }}
      </LdLink>

      <div
        class="disabled"
        :title="t('tag-propositions.item-count')"
      >
        {{ proposed.count }}
      </div>
    </div>
    <div
      v-if="showMoreCount > 0"
      :class="$style.showmore"
      @click="limit += showMoreCount"
    >
      {{ t("tag-propositions.showmore", [showMoreCount]) }}<fa-icon :icon="faAngleDoubleDown" />
    </div>
  </div>
</template>

<script setup lang="ts">
import { Item, RawTag } from '@/@types/gallery';
import { Operation } from '@/@types/operation';
import { TagIndex, TagNode, TagSearch } from '@/@types/tag';
import LdLink from '@/components/LdLink.vue';
import { useGalleryStore } from '@/store/galleryStore';
import { faAngleDoubleDown, faMinus, faPlus } from '@fortawesome/free-solid-svg-icons';
import { useVModel } from '@vueuse/core';
import { computed, PropType, ref, watch } from 'vue';
import { useI18n } from 'vue-i18n';
import { useRoute } from 'vue-router';

const props = defineProps({
  searchFilters: { type: Array<TagSearch>, required: true },
  currentTags: { type: Array<string>, required: true },
  tagsIndex: { type: Object as PropType<TagIndex>, required: true },
  category: { type: Object as PropType<TagNode>, default: null },
  showCategory: Boolean,
});
const emit = defineEmits(['update:searchFilters']);
const model = useVModel(props, 'searchFilters', emit);

const { t } = useI18n();
const route = useRoute();
const galleryStore = useGalleryStore();

const initialTagDisplayLimit = computed(() => {
  const limit = galleryStore.config?.initialTagDisplayLimit ?? 10;
  return limit >= 0 ? limit : 1000;
});

const limit = ref(initialTagDisplayLimit.value);

watch(() => route, () => (limit.value = initialTagDisplayLimit.value));

const propositions = computed<Record<string, number>>(() => {
  const propositions: Record<string, number> = {};
  const searchFilters = model.value;
  if (searchFilters.length > 0) {
    // Tags count from current search
    extractDistinctItems(searchFilters)
      .flatMap(item => item.tags)
      .map(rightmost)
      .filter(rawTag => props.tagsIndex[rawTag] && !searchFilters.find(search => search.tag === rawTag))
      .forEach(rawTag => (propositions[rawTag] = (propositions[rawTag] ?? 0) + 1));
  } else {
    // Tags count from the current directory
    props.currentTags
      .flatMap(tag => tag.split(':'))
      .map(tag => props.tagsIndex[tag])
      .filter(Boolean)
      .forEach(tagindex => (propositions[tagindex.tag] = tagindex.items.length));
  }
  return propositions;
});

const proposedTags = computed(() => {
  return Object.entries(propositions.value)
    .sort((a, b) => b[1] - a[1])
    .slice(0, limit.value)
    .map(entry => ({ rawTag: entry[0], count: entry[1] }));
});

const showMoreCount = computed(() => {
  return Object.keys(propositions.value).length - Object.keys(proposedTags.value).length;
});

const title = computed(() => {
  return props.category?.tag ?? t('panelLeft.propositions.other');
});

function extractDistinctItems(currentTags: TagSearch[]): Item[] {
  return [...new Set(currentTags.flatMap(tag => tag.items))];
}

function rightmost(tag: RawTag): RawTag {
  const dot = tag.lastIndexOf(':');
  return dot <= 0 ? tag : tag.substring(dot + 1);
}

function add(operation: Operation, rawTag: RawTag) {
  const node = props.tagsIndex[rawTag];
  const display = props.category ? `${operation}${props.category.tag}:${node.tag}` : `${operation}${node.tag}`;
  model.value.push({ ...node, parent: props.category, operation, display });
}
</script>

<style lang="scss" module>
@import "~@/assets/scss/theme";

.proposition {
  .subtitle {
    background-color: $proposed-category-bgcolor;
    width: 100%;
    padding: 0 0 6px 0;
    margin: 0;
    text-align: center;
    font-variant: small-caps;
  }
  > div {
    display: flex;
    align-items: center;
    padding-right: 7px;
    .operationTag {
      text-overflow: ellipsis;
      white-space: nowrap;
      overflow: hidden;
      flex-grow: 1;
      cursor: pointer;
    }
    .operationBtns {
      padding: 2px 7px;
      cursor: pointer;
    }
  }
  .showmore {
    display: block;
    text-align: right;
    color: $palette-300;
    cursor: pointer;
    > svg {
      margin-left: 10px;
    }
    &:hover {
      color: $link-hover;
    }
  }
}
</style>
