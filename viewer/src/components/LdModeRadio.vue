<!-- ldgallery - A static generator which turns a collection of tagged
--             pictures into a searchable web gallery.
--
-- Copyright (C) 2019-2020  Guillaume FOUET
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
-->

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
