#!/usr/bin/env node

/**
 * Script comparing the item filtering speed based on tags using different
 * methods: array scan, array query, bitwise query.
 */

function range(length) {
  return Array.from(Array(length).keys());
}

function shuffle(array) {
  for(let i = array.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * i);
    [array[i], array[j]] = [array[j], array[i]];
  }
}

function randomPermutation(length) {
  let array = range(length);
  shuffle(array);
  return array;
}

function randomSample(size, rangeLength) {
  return randomPermutation(rangeLength).slice(0, size);
}

const tagSpace = 200;
const interSlice = randomPermutation(tagSpace);
const unionSlice = randomPermutation(tagSpace);
const exclSlice = randomPermutation(tagSpace);

// 1 million items, each with 20 tags drawn from the tag space
const itemTagLists = range(1000000).map(_ => randomSample(20, tagSpace));

function scanList(inter, union, excl, inList) {
  return (inter.every(x => inList.includes(x))
    || union.some(x => inList.includes(x)))
    && !excl.some(x => inList.includes(x));
}

const itemTagSets = itemTagLists.map(list => new Set(list));

function querySet(inter, union, excl, inSet) {
  return (inter.every(x => inSet.has(x))
    || union.some(x => inSet.has(x)))
    && !excl.some(x => inSet.has(x));
}

function toBitArray(tagList) {
  let bitArray = BigInt(0);
  for (const tag of tagList)
    bitArray |= BigInt(1) << BigInt(tag);

  return bitArray;
}

const itemTagBitArrays = itemTagLists.map(toBitArray);

function queryBitArray(inter, union, excl, bitArray) {
  return (((bitArray & inter) == inter)
    || (bitArray & union))
    && !(bitArray & excl);
}

const { performance } = require('perf_hooks');

const runCounts = 5;

function test(label, searchFunc) {
  for (let searchTagCount = 1; searchTagCount <= 20; searchTagCount++) {
    const interQuery = interSlice.slice(0, searchTagCount);
    const unionQuery = unionSlice.slice(0, searchTagCount);
    const exclQuery = exclSlice.slice(0, searchTagCount);
    const startTsMs = performance.now();

    for (let i = 0; i < runCounts; i++)
      searchFunc(interQuery, unionQuery, exclQuery);

    const execMs = performance.now() - startTsMs;
    const execEachMs = execMs / runCounts;
    console.log(`${label},${searchTagCount},${execEachMs.toFixed()}`);
  }
}

console.log('method,query_size,query_time_ms');

test("list_scan", (inter, union, excl) => {
  itemTagLists.filter(tags => scanList(inter, union, excl, tags));
});

test("set_query", (inter, union, excl) => {
  itemTagSets.filter(tags => querySet(inter, union, excl, tags));
});

test("bitarray_query", (inter, union, excl) => {
  const interBit = toBitArray(inter);
  const unionBit = toBitArray(union);
  const exclBit = toBitArray(excl);
  itemTagBitArrays.filter(tags =>
    queryBitArray(interBit, unionBit, exclBit, tags));
});

