<template>
  <b-taginput
    v-model="$uiStore.currentTags"
    :placeholder="$t('tagInput.placeholder')"
    autocomplete
    ellipsis
    attached
    :data="filteredTags"
    field="display"
    type="is-black"
    icon="tag"
    size="is-medium"
    class="panelTagInput"
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
import { Operation } from "@/@types/tag/Operation";

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

  extractOperation(filter: string) {
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
    return Object.values(tags)
      .filter(node => node.tag.includes(category))
      .flatMap(node =>
        Object.values(node.children)
          .filter(child => child.tag.includes(disambiguation))
          .map(child => ({ ...child, parent: node, operation, display: `${operation}${node.tag}:${child.tag}` }))
      );
  }

  searchTagsFromFilter(tags: Tag.Index, operation: Operation, filter: string): Tag.Search[] {
    return Object.values(tags)
      .filter(node => node.tag.includes(filter))
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
.panelTagInput .autocomplete .dropdown-content {
  max-height: 300px;
}
</style>
