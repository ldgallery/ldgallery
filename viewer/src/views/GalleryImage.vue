<template>
  <div>
    <strong>Image: {{image.path}}</strong>
    <img :src="imageSrc" />
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from "vue-property-decorator";

@Component
export default class GalleryImage extends Vue {
  @Prop({ required: true }) readonly image!: Gallery.Image;

  get imageSrc() {
    return `/gallery${this.image.path}`;
  }
}
</script>

<style lang="scss">
</style>
