<template>
  <div :class="{fullscreen: $uiStore.fullscreen}">
    <div class="layout layout-top">header</div>
    <panel-left class="layout layout-left" />
    <router-view class="layout layout-content" />
    <ld-button-fullscreen />
    <b-loading :active="isLoading" is-full-page />
  </div>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import PanelLeft from "./PanelLeft.vue";

@Component({
  components: { PanelLeft },
})
export default class MainLayout extends Vue {
  isLoading: boolean = false;

  mounted() {
    this.fetchGalleryItems();
  }

  fetchGalleryItems() {
    this.isLoading = true;
    this.$galleryStore
      .fetchGalleryItems(`${process.env.VUE_APP_DATA_URL}/index.json`)
      .finally(() => (this.isLoading = false))
      .catch(this.displayError);
  }

  displayError(reason: any) {
    this.$buefy.snackbar.open({
      message: `Error ${reason}`,
      actionText: "Retry",
      position: "is-top",
      type: "is-danger",
      indefinite: true,
      onAction: this.fetchGalleryItems,
    });
  }
}
</script>

<style lang="scss">
$layout-top: 30px;
$layout-left: 250px;

body,
html {
  height: 100%;
  overflow: hidden;
  --layout-top: #{$layout-top};
  --layout-left: #{$layout-left};
}
.layout {
  position: fixed;
  transition: all 0.1s linear;
  top: 0;
  bottom: 0;
  left: 0;
  right: 0;
  &.layout-top {
    height: $layout-top;
    z-index: 1;
  }
  &.layout-left {
    top: $layout-top;
    width: $layout-left;
    z-index: 2;
  }
  &.layout-content {
    top: var(--layout-top);
    left: var(--layout-left);
    z-index: 3;
  }
}
.fullscreen {
  --layout-left: 0px;
  --layout-top: 0px;
  .layout {
    &.layout-left {
      transform: translate(-$layout-left, 0);
    }
  }
}

// temp colors
.layout {
  &.layout-top {
    background-color: darkslategray;
    color: white;
  }
  &.layout-left {
    background-color: darkblue;
    color: white;
  }
  &.layout-content {
    background-color: lightcyan;
  }
}
</style>