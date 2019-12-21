<template>
  <img class="thumbnail" :src="imageSrc" :title="item.path" />
</template>

<script lang="ts">
import { Component, Vue, Prop } from "vue-property-decorator";

@Component
export default class GalleryThumbnail extends Vue {
  @Prop({ required: true }) readonly item!: Gallery.Item;

  get imageSrc() {
    return `/gallery${this.item.thumbnail.path}`;
  }
}
</script>

<style lang="scss">
.thumbnail {
  max-width: 250px;
  max-height: 250px;
}
</style>
