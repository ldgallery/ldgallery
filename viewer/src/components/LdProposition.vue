<!-- ldgallery - A static generator which turns a collection of tagged
--             pictures into a searchable web gallery.
--
-- Copyright (C) 2019-2020  Guillaume FOUET
--               2020       Pacien TRAN-GIRARD
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
  <div class="proposition">
    <h2 v-if="showCategory && proposedTags.length" class="subtitle category">{{ title }}</h2>
    <div v-for="proposed in proposedTags" :key="proposed.rawTag">
      <a
        class="operation-btns link"
        :title="$t('tag-propositions.substraction')"
        @click="add(Operation.SUBSTRACTION, proposed.rawTag)"
      >
        <fa-icon icon="minus" alt="[-]" />
      </a>

      <a
        class="operation-btns link"
        :title="$t('tag-propositions.addition')"
        @click="add(Operation.ADDITION, proposed.rawTag)"
      >
        <fa-icon icon="plus" alt="[+]" />
      </a>

      <a
        class="operation-tag link"
        :title="$t('tag-propositions.intersection')"
        @click="add(Operation.INTERSECTION, proposed.rawTag)"
        >{{ proposed.rawTag }}</a
      >

      <div class="disabled" :title="$t('tag-propositions.item-count')">{{ proposed.count }}</div>
    </div>
    <div v-if="showMoreCount > 0" class="showmore" @click="limit += showMoreCount">
      {{ $t("tag-propositions.showmore", [showMoreCount]) }}<fa-icon icon="angle-double-down" />
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop, PropSync, Watch } from "vue-property-decorator";
import { Operation } from "@/@types/Operation";

@Component
export default class LdProposition extends Vue {
  @Prop() readonly category?: Tag.Node;
  @Prop({ type: Boolean, required: true }) readonly showCategory!: boolean;
  @Prop({ type: Array, required: true }) readonly currentTags!: string[];
  @Prop({ required: true }) readonly tagsIndex!: Tag.Index;
  @PropSync("searchFilters", { type: Array, required: true }) model!: Tag.Search[];

  readonly INITIAL_TAG_DISPLAY_LIMIT = this.getInitialTagDisplayLimit();

  limit: number = this.INITIAL_TAG_DISPLAY_LIMIT;

  getInitialTagDisplayLimit() {
    const limit = this.$galleryStore.config?.initialTagDisplayLimit ?? 10;
    return limit > 0 ? limit : 1000;
  }

  @Watch("$route")
  onRouteChange() {
    this.limit = this.INITIAL_TAG_DISPLAY_LIMIT;
  }

  get Operation() {
    return Operation;
  }

  get propositions(): Record<string, number> {
    const propositions: Record<string, number> = {};
    if (this.model.length > 0) {
      // Tags count from current search
      this.extractDistinctItems(this.model)
        .flatMap(item => item.tags)
        .map(this.rightmost)
        .filter(rawTag => this.tagsIndex[rawTag] && !this.model.find(search => search.tag === rawTag))
        .forEach(rawTag => (propositions[rawTag] = (propositions[rawTag] ?? 0) + 1));
    } else {
      // Tags count from the current directory
      this.currentTags
        .flatMap(tag => tag.split(":"))
        .map(tag => this.tagsIndex[tag])
        .filter(Boolean)
        .forEach(tagindex => (propositions[tagindex.tag] = tagindex.items.length));
    }
    return propositions;
  }

  get proposedTags() {
    return Object.entries(this.propositions)
      .sort((a, b) => b[1] - a[1])
      .slice(0, this.limit)
      .map(entry => ({ rawTag: entry[0], count: entry[1] }));
  }

  get showMoreCount(): number {
    return Object.keys(this.propositions).length - Object.keys(this.proposedTags).length;
  }

  get title() {
    return this.category?.tag ?? this.$t("panelLeft.propositions.other");
  }

  extractDistinctItems(currentTags: Tag.Search[]): Gallery.Item[] {
    return [...new Set(currentTags.flatMap(tag => tag.items))];
  }

  rightmost(tag: Gallery.RawTag): Gallery.RawTag {
    const dot = tag.lastIndexOf(":");
    return dot <= 0 ? tag : tag.substr(dot + 1);
  }

  add(operation: Operation, rawTag: Gallery.RawTag) {
    const node = this.tagsIndex[rawTag];
    const display = this.category ? `${operation}${this.category.tag}:${node.tag}` : `${operation}${node.tag}`;
    this.model.push({ ...node, parent: this.category, operation, display });
  }
}
</script>

<style lang="scss">
@import "~@/assets/scss/theme.scss";

.proposition {
  .subtitle {
    background-color: $proposed-category-bgcolor;
    width: 100%;
    padding: 0 0 6px 0;
    margin: 0;
    text-align: center;
    font-variant: small-caps;
  }
  > div {
    display: flex;
    align-items: center;
    padding-right: 7px;
    .operation-tag {
      text-overflow: ellipsis;
      white-space: nowrap;
      overflow: hidden;
      flex-grow: 1;
      cursor: pointer;
    }
    .operation-btns {
      padding: 2px 7px;
      cursor: pointer;
    }
  }
  .showmore {
    display: block;
    text-align: right;
    color: $palette-300;
    cursor: pointer;
    > svg {
      margin-left: 10px;
    }
    &:hover {
      color: $link-hover;
    }
  }
}
</style>
