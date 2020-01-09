<template>
  <div>
    <img :src="pictureSrc" />
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from "vue-property-decorator";

@Component
export default class GalleryPicture extends Vue {
  @Prop({ required: true }) readonly picture!: Gallery.Picture;

  get pictureSrc() {
    return `${process.env.VUE_APP_DATA_URL}${this.picture.properties.resource}`;
  }
}
</script>

<style lang="scss">
</style>
