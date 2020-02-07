<!-- ldgallery - A static generator which turns a collection of tagged
--             pictures into a searchable web gallery.
--
-- Copyright (C) 2019-2020  Guillaume FOUET
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
  <b-taginput
    v-model="$uiStore.currentTags"
    :placeholder="$t('tagInput.placeholder')"
    autocomplete
    ellipsis
    attached
    :data="filteredTags"
    field="display"
    size="is-medium"
    class="paneltag-input"
    @typing="searchTags"
    @add="onAdd"
    @remove="onRemove"
  >
    <template slot-scope="props">{{displayOption(props.option)}}</template>
    <template slot="empty">{{$t('tagInput.nomatch')}}</template>
  </b-taginput>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import { Operation } from "@/@types/Operation";
import Tools from "@/tools";

@Component
export default class LdTagInput extends Vue {
  filteredTags: Tag.Search[] = [];

  onAdd(e: any) {
    this.$uiStore.mode = "search";
  }

  onRemove() {
    if (this.$uiStore.currentTags.length === 0) this.$uiStore.mode = "navigation";
  }

  displayOption(option: Tag.Search): string {
    return `${option.display} (${option.items.length})`;
  }

  extractOperation(filter: string): Operation {
    const first = filter.slice(0, 1);
    switch (first) {
      case Operation.ADDITION:
      case Operation.SUBSTRACTION:
        return first;
      default:
        return Operation.INTERSECTION;
    }
  }

  searchTags(filter: string) {
    const tags = this.$galleryStore.tags;
    let search: Tag.Search[] = [];
    if (tags && filter) {
      const operation = this.extractOperation(filter);
      if (operation !== Operation.INTERSECTION) filter = filter.slice(1);
      if (filter.includes(":")) {
        const filterParts = filter.split(":");
        search = this.searchTagsFromFilterWithCategory(tags, operation, filterParts[0], filterParts[1]);
      } else {
        search = this.searchTagsFromFilter(tags, operation, filter);
      }
    }
    this.filteredTags = this.cleanupAndSort(search);
  }

  searchTagsFromFilterWithCategory(
    tags: Tag.Index,
    operation: Operation,
    category: string,
    disambiguation: string
  ): Tag.Search[] {
    disambiguation = Tools.normalize(disambiguation);
    return Object.values(tags)
      .filter(node => node.tag.includes(category))
      .flatMap(node =>
        Object.values(node.children)
          .filter(child => child.tagfiltered.includes(disambiguation))
          .map(child => ({ ...child, parent: node, operation, display: `${operation}${node.tag}:${child.tag}` }))
      );
  }

  searchTagsFromFilter(tags: Tag.Index, operation: Operation, filter: string): Tag.Search[] {
    filter = Tools.normalize(filter);
    return Object.values(tags)
      .filter(node => node.tagfiltered.includes(filter))
      .map(node => ({ ...node, operation, display: `${operation}${node.tag}` }));
  }

  cleanupAndSort(search: Tag.Search[]): Tag.Search[] {
    const currentTags = this.$uiStore.currentTags;
    return search
      .filter(node => !currentTags.find(currentTag => currentTag.tag === node.tag))
      .sort((a, b) => b.items.length - a.items.length);
  }
}
</script>

<style lang="scss">
@import "@/assets/scss/theme.scss";

.paneltag-input {
  .taginput-container.is-focusable {
    box-shadow: none !important;
    border-color: transparent !important;

    &.is-focused {
      border-color: $input-active-outline-color !important;
    }
  }

  .tag {
    background-color: $input-tag-background-color;

    &.is-delete {
      background-color: $input-tag-delete-background-color !important;

      &:hover {
        color: $link-hover;
      }
    }
  }

  .paneltag-input .autocomplete .dropdown-content {
    max-height: 300px;
  }
}
</style>
