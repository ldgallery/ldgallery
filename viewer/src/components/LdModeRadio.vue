<template>
  <div class="flex">
    <b-radio-button v-model="$uiStore.mode" native-value="navigation" type="is-green">
      <fa-icon icon="folder" />
      <span>{{$t('mode.navigation')}}</span>
    </b-radio-button>
    <b-radio-button v-model="$uiStore.mode" native-value="search" type="is-purple">
      <fa-icon icon="search" />
      <span>{{$t('mode.search')}}</span>
    </b-radio-button>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";

@Component
export default class LdModeRadio extends Vue {}
</script>

<style lang="scss">
</style>
