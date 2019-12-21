<template>
  <div>
    <strong>Directory: {{directory.path}}</strong>
    <div v-for="(item, index) in directory.properties.items" :key="item.path">
      <router-link :to="item.path">Thumbnail: {{index}}-{{item.path}}</router-link>
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from "vue-property-decorator";

@Component
export default class GalleryDirectory extends Vue {
  @Prop({ type: Array, required: true }) readonly directory!: Gallery.Directory;
}
</script>

<style lang="scss">
</style>
