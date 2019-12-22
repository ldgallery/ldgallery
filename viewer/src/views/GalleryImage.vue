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
    return `${process.env.VUE_APP_DATA_URL}${this.image.path}`;
  }
}
</script>

<style lang="scss">
</style>
