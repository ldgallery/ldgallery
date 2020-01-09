<template>
  <div class="flex">
    <div v-for="(item) in items" :key="item.path">
      <router-link :to="item.path" @click.native="$uiStore.setModeNavigation()">
        <gallery-thumbnail :item="item" />
      </router-link>
    </div>
    <div v-if="items.length===0">{{$t('search.no-results')}}</div>
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from "vue-property-decorator";
import GalleryThumbnail from "./GalleryThumbnail.vue";

@Component({
  components: { GalleryThumbnail },
})
export default class GalleryPicture extends Vue {
  @Prop({ required: true }) readonly items!: Gallery.Item[];
}
</script>

<style lang="scss">
</style>
