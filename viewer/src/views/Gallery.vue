<template>
  <div id="test-flex-col">
    <gallery-directory v-if="isDirectory" :directory="currentItem" />
    <gallery-image v-if="isImage" :image="currentItem" />
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from "vue-property-decorator";
import GalleryDirectory from "./GalleryDirectory.vue";
import GalleryImage from "./GalleryImage.vue";

@Component({
  components: { GalleryDirectory, GalleryImage },
})
export default class Root extends Vue {
  @Prop(String) readonly pathMatch!: string;

  get isDirectory(): boolean {
    return this.checkType("directory");
  }

  get isImage(): boolean {
    return this.checkType("image");
  }

  get currentItem(): Gallery.Item | null {
    const galleryItems = this.$galleryStore.galleryItems;
    if (galleryItems) return this.searchCurrentItem(galleryItems, this.pathMatch);
    return null;
  }

  // ---

  private searchCurrentItem(item: Gallery.Item, currentPath: string): Gallery.Item | null {
    if (currentPath === item.path) return item;
    if (item.properties.type === "directory" && currentPath.startsWith(item.path)) {
      const itemFound = item.properties.items
        .map(item => this.searchCurrentItem(item, currentPath))
        .find(item => Boolean(item));
      return itemFound || null;
    }
    return null;
  }

  private checkType(type: string): boolean {
    return (this.currentItem && this.currentItem.properties.type === type) || false;
  }
}
</script>

<style lang="scss">
#test-flex-col {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
}
</style>
