<template>
  <div>
    <strong>Directory: {{directory.path}}</strong>
    <div class="flex">
      <div v-for="(item) in directory.properties.items" :key="item.path">
        <router-link :to="item.path">
          <gallery-thumbnail :item="item" />
        </router-link>
      </div>
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from "vue-property-decorator";
import GalleryThumbnail from "./GalleryThumbnail.vue";

@Component({
  components: { GalleryThumbnail },
})
export default class GalleryDirectory extends Vue {
  @Prop({ required: true }) readonly directory!: Gallery.Directory;
}
</script>

<style lang="scss">
</style>
