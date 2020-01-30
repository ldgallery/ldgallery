<!-- ldgallery - A static generator which turns a collection of tagged
--             pictures into a searchable web gallery.
--
-- Copyright (C) 2019-2020  Guillaume FOUET
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
  <div>
    <div v-for="proposed in proposedTags" :key="proposed.rawTag" class="proposition link">
      <fa-icon icon="minus" @click="add(Operation.SUBSTRACTION, proposed.rawTag)" />
      <span
        @click="add(Operation.INTERSECTION, proposed.rawTag)"
      >{{proposed.rawTag}}&nbsp;x{{proposed.count}}</span>
      <fa-icon icon="plus" @click="add(Operation.ADDITION, proposed.rawTag)" />
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import { Operation } from "@/@types/tag/Operation";

@Component
export default class LdTagInput extends Vue {
  get Operation() {
    return Operation;
  }

  get proposedTags() {
    const currentTags = this.$uiStore.currentTags;
    let propositions: { [index: string]: number } = {};
    if (currentTags.length > 0) {
      // Tags count from current search
      this.extractDistinctItems(currentTags)
        .flatMap(item => item.tags)
        .map(this.rightmost)
        .filter(rawTag => !currentTags.find(currentTag => currentTag.tag === rawTag))
        .forEach(rawTag => (propositions[rawTag] = (propositions[rawTag] ?? 0) + 1));
    } else {
      // Tags count from the whole gallery
      Object.entries(this.$galleryStore.tags).forEach(entry => (propositions[entry[0]] = entry[1].items.length));
    }

    return Object.entries(propositions)
      .sort((a, b) => b[1] - a[1])
      .map(entry => ({ rawTag: entry[0], count: entry[1] }));
  }

  extractDistinctItems(currentTags: Tag.Search[]): Gallery.Item[] {
    return [...new Set(currentTags.flatMap(tag => tag.items))];
  }

  rightmost(tag: Gallery.RawTag): Gallery.RawTag {
    const dot = tag.lastIndexOf(".");
    return dot <= 0 ? tag : tag.substr(dot + 1);
  }

  add(operation: Operation, rawTag: Gallery.RawTag) {
    const node = this.$galleryStore.tags[rawTag];
    const search: Tag.Search = { ...node, operation, display: `${operation}${node.tag}` };
    this.$uiStore.currentTags.push(search);
    this.$uiStore.mode = "search";
  }
}
</script>

<style lang="scss">
@import "@/assets/scss/theme.scss";

.proposition {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin: 10px;
  cursor: pointer;
}
.proposition span {
  padding: 0 10px;
}
</style>
