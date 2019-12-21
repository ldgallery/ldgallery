<template>
  <div>
    <strong>Image: {{image.path}}</strong>
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from "vue-property-decorator";

@Component
export default class GalleryDirectory extends Vue {
  @Prop({ required: true }) readonly image!: Gallery.Image;
}
</script>

<style lang="scss">
</style>
