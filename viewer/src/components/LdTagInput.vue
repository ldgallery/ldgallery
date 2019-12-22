<template>
  <b-taginput
    v-model="$uiStore.currentTags"
    :placeholder="$t('tagInput.placeholder')"
    autocomplete
    :data="filteredTags"
    field="tag"
    type="is-black"
    size="is-large"
    class="panelTagInput"
    @typing="getFilteredTags"
  >
    <template slot-scope="props">{{props.option.tag}} ({{props.option.items.length}})</template>
    <template slot="empty">{{$t('tagInput.nomatch')}}</template>
  </b-taginput>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";

@Component
export default class LdTagInput extends Vue {
  filteredTags: Tag.Node[] = [];

  getFilteredTags(filter: string) {
    const tags = this.$galleryStore.tags;
    if (tags && filter)
      this.filteredTags = Object.values(tags)
        .filter(node => node.tag.includes(filter))
        .sort((a, b) => b.items.length - a.items.length);
    else this.filteredTags = [];
  }
}
</script>

<style lang="scss">
.panelTagInput .autocomplete .dropdown-content {
  max-height: 300px;
}
</style>
