<template>
  <div :class="{fullscreen: $uiStore.fullscreen}">
    <div class="layout layout-top">header</div>
    <div class="layout layout-left">panel</div>
    <router-view class="layout layout-content" />
    <ld-button-fullscreen />
  </div>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";

@Component
export default class LdGallery extends Vue {}
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