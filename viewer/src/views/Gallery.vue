<template>
  <div id="test-flex-col">
    <div v-if="isDirectory">
      <strong>Directory: {{currentItem.path}}</strong>
      <div v-for="(item, index) in currentItem.properties.items" :key="item.path">
        <router-link :to="item.path">Thumbnail: {{index}}-{{item.path}}</router-link>
      </div>
    </div>
    <div v-if="isImage">Image: {{currentItem.path}}</div>
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from "vue-property-decorator";

@Component
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

  private checkType(type: string) {
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
