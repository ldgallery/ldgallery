<template>
  <div>
    <b-field :label="$t('panelLeft.title')">
      <ld-tag-input />
    </b-field>
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
