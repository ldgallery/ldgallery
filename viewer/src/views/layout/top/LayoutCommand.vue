<!-- ldgallery - A static generator which turns a collection of tagged
--             pictures into a searchable web gallery.
--
-- Copyright (C) 2019-2022  Guillaume FOUET
--               2020-2022  Pacien TRAN-GIRARD
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
  <div
    class="flex"
    :class="$style.commandBtns"
  >
    <LdLink
      :title="t('command.search.slider')"
      tabindex="10"
      @click="uiStore.toggleFullWidth()"
    >
      <fa-icon
        :icon="commandToggleSearchPanelIcon"
        size="lg"
      />
    </LdLink>

    <LdLink
      v-if="itemResourceUrl"
      :title="t('command.download')"
      :download="navigation.getFileName(props.item)"
      :href="itemResourceUrl"
      :tabindex="20"
    >
      <fa-icon
        :icon="faFileDownload"
        size="lg"
      />
    </LdLink>
    <LayoutCommandSort
      v-else
      :tabindex="20"
    />

    <LdLink
      :class="{ disabled: isEntryPoint(), [$style.commandSecondary]: true }"
      :title="t('command.back')"
      tabindex="30"
      @click="isEntryPoint() || router.back()"
    >
      <fa-icon
        :icon="faArrowLeft"
        size="lg"
      />
    </LdLink>
    <LdLink
      :to="parent"
      :class="{ disabled: isRoot }"
      :title="t('command.parent')"
      tabindex="40"
    >
      <fa-icon
        :icon="faFolder"
        size="xs"
      />
      <fa-icon
        :icon="faLevelUpAlt"
        size="lg"
      />
    </LdLink>
  </div>
</template>

<script setup lang="ts">
import { Item } from '@/@types/gallery';
import LdLink from '@/components/LdLink.vue';
import { useNavigation } from '@/services/navigation';
import { useItemResource } from '@/services/ui/ldItemResourceUrl';
import { useUiStore } from '@/store/uiStore';
import {
  faAngleDoubleLeft,
  faArrowLeft,
  faFileDownload,
  faFolder,
  faLevelUpAlt,
  faSearch,
} from '@fortawesome/free-solid-svg-icons';
import { computedEager } from '@vueuse/shared';
import { computed, PropType, toRef } from 'vue';
import { useI18n } from 'vue-i18n';
import { useRoute, useRouter } from 'vue-router';
import LayoutCommandSort from './LayoutCommandSort.vue';

const props = defineProps({
  currentItemPath: { type: Array<Item>, required: true },
  item: { type: Object as PropType<Item>, required: true },
});

const { t } = useI18n();
const route = useRoute();
const router = useRouter();
const uiStore = useUiStore();
const navigation = useNavigation();
const { itemResourceUrl } = useItemResource(toRef(props, 'item'));

const commandToggleSearchPanelIcon = computed(() => uiStore.fullWidth ? faSearch : faAngleDoubleLeft);
const isRoot = computedEager(() => props.currentItemPath.length <= 1 && !uiStore.searchMode);
const parent = computed(() => {
  if (uiStore.searchMode) return decodeURIComponent(route.path);
  const ln = props.currentItemPath.length;
  if (ln > 1) return props.currentItemPath[ln - 2];
  return '';
});

function isEntryPoint() {
  return history.state?.ldgallery === 'ENTRYPOINT'; // Set by MainLayout.vue
}
</script>

<style lang="scss" module>
@import "~@/assets/scss/theme";

.commandBtns {
  background-color: $command-buttons-bgcolor;
  justify-content: space-around;
  vertical-align: middle;
  align-items: center;
  flex: 0 0 $layout-left;
  user-select: none;

  > a {
    // normalise icon active boxes
    width: $layout-top;
    line-height: calc($layout-top - 4px); // 4px For the focus border
    text-align: center;
    vertical-align: middle;
  }

  @media only screen and (max-width: $tablet) {
    flex: 0 1;

    > .commandSecondary {
      display: none;
    }
  }
}
</style>
