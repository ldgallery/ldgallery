<template>
  <fa-icon icon="expand-arrows-alt" class="button-fullscreen" @click="$uiStore.toggleFullscreen()" />
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";

@Component
export default class LdButtonFullscreen extends Vue {}
</script>

<style lang="scss">
.button-fullscreen {
  position: fixed;
  top: 0;
  right: 0;
  margin: 3px 10px;
  opacity: 50%;
  font-size: 1.5em;
  color: white;
  mix-blend-mode: difference;
  cursor: pointer;
  z-index: 4;
}
</style>
