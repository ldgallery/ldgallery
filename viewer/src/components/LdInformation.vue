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
  <div v-if="item" class="flex-column" :class="$style.infopanel">
    <div v-if="item.title" :class="$style.title">{{ item.title }}</div>
    <time v-if="item.datetime" :datetime="item.datetime" :class="$style.datetime">{{ formatDate }}</time>
    <Markdown v-if="item.description" :class="$style.description" :markdown="item.description" />
  </div>
</template>

<script lang="ts">
import { Item } from "@/@types/gallery";
import { Markdown } from "@/components/async";
import { Component, Prop, Vue } from "vue-property-decorator";

@Component({
  components: {
    Markdown,
  },
})
export default class LdInformation extends Vue {
  @Prop({ required: true }) readonly item!: Item;

  get formatDate() {
    const date = this.item.datetime.substr(0, 10);
    const time = this.item.datetime.substr(11, 5);
    return `${date} ${time}`;
  }
}
</script>

<style lang="scss" module>
@import "~@/assets/scss/theme.scss";

.infopanel {
  padding: 2px 2px 7px 7px;
  overflow-wrap: break-word;

  .title {
    font-weight: bold;
  }
  .datetime {
    font-size: 0.9em;
    color: $palette-300;
  }
  .description {
    padding-bottom: 7px;
    > * {
      margin-top: 5px;
    }
    ul,
    ol {
      margin-left: 1em;
    }
    ul {
      list-style-type: disc;
    }
    a {
      color: $palette-200;
      &:hover {
        color: $palette-050;
        text-decoration: underline;
      }
    }
  }
}
</style>
