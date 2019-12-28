<template>
  <div class="flex-column">
    <h1>{{$t('panelLeft.mode')}}</h1>
    <ld-mode-radio />
    <h1>{{$t('panelLeft.filters')}}</h1>
    <ld-tag-input />
    <h1>{{$t('panelLeft.propositions')}}</h1>
    <ld-proposition />
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from "vue-property-decorator";

@Component
export default class PanelLeft extends Vue {}
</script>

<style lang="scss">
.label {
  color: white;
}
</style>
