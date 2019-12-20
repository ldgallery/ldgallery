<template>
  <div>Hello World!</div>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";

@Component
export default class Root extends Vue {}
</script>

<style scoped lang="scss">
div {
  display: flex;
  align-items: center;
  justify-content: center;
}
</style>
