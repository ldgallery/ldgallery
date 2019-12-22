<template>
  <div>
    <gallery-search v-if="$uiStore.isModeSearch" :items="currentSearch" />
    <gallery-directory v-else-if="isDirectory" :directory="currentItem" />
    <gallery-image v-else-if="isImage" :image="currentItem" />
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from "vue-property-decorator";
import GallerySearch from "./GallerySearch.vue";
import GalleryDirectory from "./GalleryDirectory.vue";
import GalleryImage from "./GalleryImage.vue";
import GalleryStore from "../store/galleryStore";

@Component({
  components: { GallerySearch, GalleryDirectory, GalleryImage },
})
export default class Gallery extends Vue {
  @Prop(String) readonly pathMatch!: string;

  get isDirectory(): boolean {
    return this.checkType("directory");
  }

  get isImage(): boolean {
    return this.checkType("image");
  }

  // Results of the search (by tags)
  get currentSearch(): Gallery.Item[] {
    const currentTags = this.$uiStore.currentTags;
    let items = new Set<Gallery.Item>();
    currentTags.flatMap(tag => tag.items).forEach(item => items.add(item));
    return [...items];
  }

  // Item pointed by the URL (navigation)
  get currentItem(): Gallery.Item | null {
    const galleryItemsRoot = this.$galleryStore.galleryItemsRoot;
    if (galleryItemsRoot) return GalleryStore.searchCurrentItem(galleryItemsRoot, this.pathMatch);
    return null;
  }

  // ---

  private checkType(type: string): boolean {
    return (this.currentItem && this.currentItem.properties.type === type) || false;
  }
}
</script>

<style lang="scss">
</style>
