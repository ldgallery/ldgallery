<template>
  <div :class="{fullscreen: $uiStore.fullscreen}">
    <div class="layout top">header</div>
    <div class="layout left">panel</div>
    <router-view class="layout content" />
    <fa-icon
      icon="expand-arrows-alt"
      class="layout button-fullscreen"
      @click="$uiStore.toggleFullscreen()"
    />
  </div>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";

@Component
export default class LdGallery extends Vue {}
</script>

<style lang="scss">
body,
html {
  height: 100%;
  overflow: hidden;
  --layout-left: 250px;
  --layout-top: 30px;
}
.layout {
  position: fixed;
  transition: all 0.1s linear;
  &.top {
    top: 0;
    left: 0;
    right: 0;
    height: var(--layout-top);
  }
  &.left {
    top: var(--layout-top);
    bottom: 0;
    left: 0;
    width: var(--layout-left);
  }
  &.content {
    top: var(--layout-top);
    left: var(--layout-left);
    right: 0;
    bottom: 0;
  }
  &.button-fullscreen {
    top: 0;
    right: 0;
    margin: 3px 10px;
    opacity: 50%;
    font-size: 1.5em;
    color: white;
    mix-blend-mode: difference;
    cursor: pointer;
  }
}
.fullscreen {
  --layout-left: 0px;
  --layout-top: 0px;
}

// temp colors
.layout {
  &.top {
    background-color: darkslategray;
    color: white;
  }
  &.left {
    background-color: darkblue;
    color: white;
  }
  &.content {
    background-color: lightcyan;
  }
}
</style>