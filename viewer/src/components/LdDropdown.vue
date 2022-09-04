<template>
  <div style="position:relative;">
    <Transition name="fade">
      <div
        v-if="model"
        ref="dropdown"
        class="scrollbar"
        :class="$style.dropdown"
        v-bind="attrs"
      >
        <div
          v-for="(option,idx) in props.list"
          :key="listKey ? option[props.listKey] : idx"
          :tabindex="props.tabindexRoot + idx"
          @click="emit('select', option)"
          @keypress.enter.space="emit('select', option)"
        >
          <slot
            name="option"
            :option="option"
          />
        </div>
        <slot
          v-if="!props.list.length"
          name="empty"
        />
      </div>
    </Transition>
  </div>
</template>

<script setup lang="ts">
import { onClickOutside, onKeyStroke, useVModel } from '@vueuse/core';
import { ref, useAttrs, watch } from 'vue';

const props = defineProps({
  modelValue: { type: Boolean, required: true },
  // Vue 3 currently won't allow generics in props
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  list: { type: Array<any>, required: true },
  listKey: { type: [String, Number], default: null },
  tabindexRoot: { type: Number, required: true },
});
const emit = defineEmits(['update:modelValue', 'select', 'opening', 'closing']);
const model = useVModel(props, 'modelValue', emit);

const attrs = useAttrs();

const dropdown = ref();

watch(() => model.value, value => {
  if (value) emit('opening');
  else emit('closing');
});

onClickOutside(dropdown, closeDropdown);
onKeyStroke('Escape', closeDropdown);

function closeDropdown() {
  setTimeout(() => (model.value = false));
}
</script>
<script lang="ts">
export default {
  inheritAttrs: false,
};
</script>

<style lang="scss" module>
@import "~@/assets/scss/theme";

.dropdown {
  position: absolute;
  left: 0;
  z-index: 99;
  width: $layout-left;
  color: $input-color;
  background-color: $dropdown-item-color;
  padding: 4px 0px;
  > div {
    padding: 4px 0;
    margin: 2px; // For the focus border
    cursor: pointer;
    &:hover {
      background-color: $dropdown-item-hover-color;
    }
    &:focus {
      outline: solid 1px $button-active-color;
    }
  }
}
  </style>
