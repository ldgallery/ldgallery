<template>
  <b-taginput
    v-model="$uiStore.currentTags"
    :placeholder="$t('tagInput.placeholder')"
    autocomplete
    ellipsis
    attached
    :data="filteredTags"
    field="tag"
    type="is-black"
    icon="tag"
    class="panelTagInput"
    @typing="searchTags"
  >
    <template slot-scope="props">{{displayOption(props.option)}}</template>
    <template slot="empty">{{$t('tagInput.nomatch')}}</template>
  </b-taginput>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";

@Component
export default class LdTagInput extends Vue {
  filteredTags: Tag.Search[] = [];

  displayOption(option: Tag.Search): string {
    return `${option.tag} (${option.items.length})`;
  }

  searchTags(filter: string) {
    const tags = this.$galleryStore.tags;
    let search: Tag.Search[] = [];
    if (tags && filter) {
      if (filter.includes(":")) {
        const filterParts = filter.split(":");
        search = this.searchTagsFromFilterWithCategory(tags, filterParts[0], filterParts[1]);
      } else {
        search = this.searchTagsFromFilter(tags, filter);
      }
    }
    this.filteredTags = this.cleanupAndSort(search);
  }

  searchTagsFromFilterWithCategory(tags: Tag.Index, category: string, disambiguation: string): Tag.NodeWithParent[] {
    return Object.values(tags)
      .filter(node => node.tag.includes(category))
      .flatMap(node =>
        Object.values(node.children)
          .filter(child => child.tag.includes(disambiguation))
          .map(child => ({ ...child, parent: node, tag: `${node.tag}:${child.tag}` }))
      );
  }

  searchTagsFromFilter(tags: Tag.Index, filter: string): Tag.Node[] {
    return Object.values(tags).filter(node => node.tag.includes(filter));
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
