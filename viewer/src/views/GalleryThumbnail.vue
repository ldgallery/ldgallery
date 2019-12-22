<template>
  <div>
    <img v-if="item.thumbnail" class="thumbnail" :src="imageSrc" :title="item.path" />
    <div v-else class="flex-column flex-center">
      <fa-icon icon="folder" class="fa-4x" />
      {{item.path}}
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from "vue-property-decorator";

@Component
export default class GalleryThumbnail extends Vue {
  @Prop({ required: true }) readonly item!: Gallery.Item;

  get imageSrc() {
    return `${process.env.VUE_APP_DATA_URL}${this.item.thumbnail}`;
  }
}
</script>

<style lang="scss">
.thumbnail {
  max-width: 250px;
  max-height: 250px;
}
</style>
