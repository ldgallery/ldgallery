<!--
-- ldgallery - A static generator which turns a collection of tagged
--             pictures into a searchable web gallery.
--
-- Copyright (C) 2020  Pacien TRAN-GIRARD
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
  <div class="container-vh-centering">
    <div :class="$style.content">
      <fa-icon :class="$style.icon" :icon="icon" size="6x" />
      <div :class="$style.message" v-html="message"></div>
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from "vue-property-decorator";

@Component export default class LdError extends Vue {
  @Prop({ required: true, type: String }) readonly icon!: string;
  @Prop({ required: true, type: String }) readonly message!: string;
}
</script>

<style lang="scss" module>
@import "~@/assets/scss/theme.scss";

.content {
  text-align: center;

  & > *:not(last-child) {
    margin-bottom: 0.15em;
  }
}

.icon {
  color: $disabled-color;
}

.message {
  color: $text-light;
}
</style>
