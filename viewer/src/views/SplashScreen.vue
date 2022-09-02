<!-- ldgallery - A static generator which turns a collection of tagged
--             pictures into a searchable web gallery.
--
-- Copyright (C) 2019-2022  Guillaume FOUET
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
  <LdLoading v-if="isFetching" />
  <div
    v-else-if="!error && data"
    :class="$style.splashscreen"
    class="scrollbar"
  >
    <LdMarkdown
      :style="config.style"
      class="flex-grow-1"
      :markdown="data"
    />
    <button
      tabindex="1"
      autofocus
      @click="emit('validation')"
    >
      {{ buttonAcknowledgeLabel }}
    </button>
  </div>
</template>

<script setup lang="ts">
import { LdMarkdown } from '@/components/async';
import LdLoading from '@/components/LdLoading.vue';
import { useLdFetch } from '@/services/api/ldFetch';
import { useUiStore } from '@/store/uiStore';
import { computed, readonly } from 'vue';
import { useI18n } from 'vue-i18n';

const emit = defineEmits(['validation']);

const { t } = useI18n();
const uiStore = useUiStore();

const config = readonly(uiStore.splashScreenConfig ?? {});

const buttonAcknowledgeLabel = computed(() => config.buttonAcknowledgeLabel ?? t('splashScreen.button.acknowledge'));
const itemResourceUrl = computed(() => `${process.env.VUE_APP_DATA_URL}${config.resource}?${config.acknowledgmentKey ?? ''}`);

const { isFetching, data, error } = useLdFetch(itemResourceUrl).text();
</script>

<style lang="scss" module>
.splashscreen {
  display: flex;
  flex-flow: column;
  align-items: center;
  padding: 32px;

  button {
    font-size: 1.5rem;
    margin-top: 1em;
  }
}
</style>
