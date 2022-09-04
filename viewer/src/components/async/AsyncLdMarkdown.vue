<!--
-- ldgallery - A static generator which turns a collection of tagged
--             pictures into a searchable web gallery.
--
-- Copyright (C) 2021  Guillaume FOUET
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

<!-- eslint-disable vue/no-v-html -->
<template>
  <div
    :class="$style.markdown"
    v-html="html"
  />
</template>

<script setup lang="ts">
import { marked } from 'marked';
import { computed } from 'vue';

const props = defineProps({
  markdown: { type: String, required: true },
});

const html = computed(() => marked(props.markdown));
</script>

<style lang="scss" module>
.markdown {
  color: white;
  line-height: 1.7;
  word-wrap: break-word;

  a {
    color: #9bcdff;
    text-decoration: none;
  }

  hr {
    background-color: #666;
  }

  p,
  blockquote,
  ul,
  ol,
  dl,
  table,
  pre {
    margin: 15px 0;
  }

  ul,
  ol {
    padding-left: 30px;
  }

  h1 {
    border-bottom: 1px solid #888;
    font-size: 2.5em;
  }

  h2 {
    border-bottom: 1px solid #666;
    font-size: 2em;
  }

  h3 {
    font-size: 1.5em;
  }

  h4 {
    font-size: 1.2em;
  }

  h5 {
    font-size: 1em;
  }

  h6 {
    color: #777;
    font-size: 1em;
  }

  h1,
  h2,
  h3,
  h4,
  h5,
  h6 {
    font-weight: bold;
    margin: 1em 0 15px 0;
  }

  h1 + p,
  h2 + p,
  h3 + p {
    margin-top: 10px;
  }

  pre {
    color: white;
    background-color: #2e4049;
  }

  code {
    @extend pre;
    font-family: Consolas, "Liberation Mono", Courier, monospace;
    font-size: 0.8em;
    white-space: pre;
  }
}
</style>
