<template>
  <div
    class="hero-carousel-container"
    v-if="isMounted && screenshots.length > 0"
    @pointerdown="onPointerDown"
    @pointermove="onPointerMove"
  >
    <Swiper
      :modules="modules"
      effect="coverflow"
      :grabCursor="true"
      :centeredSlides="true"
      :slidesPerView="'auto'"
      :loop="true"
      :initialSlide="initialSlideIndex"
      :autoplay="{
        delay: 2500,
        disableOnInteraction: false,
        pauseOnMouseEnter: true
      }"
      :coverflowEffect="{
        rotate: 30,
        stretch: 0,
        depth: 100,
        modifier: 1,
        slideShadows: false
      }"
      @swiper="onSwiper"
      @slideChange="onSlideChange"
      class="hero-swiper"
    >
      <SwiperSlide
        v-for="(item, index) in screenshots"
        :key="index"
        class="hero-slide"
      >
        <div class="image-wrapper" @click="handleImageClick($event, item)">
          <img :src="item.src" :alt="item.alt" />
        </div>
      </SwiperSlide>
    </Swiper>

    <Teleport to="body">
      <div
        v-if="zoomedItem"
        class="hero-zoom-overlay"
        @click="closeZoom"
      >
        <img
          :src="zoomedItem.src"
          :alt="zoomedItem.alt"
          class="hero-zoomed-image"
        />
      </div>
    </Teleport>
  </div>
</template>

<script setup lang="ts">
import { ref, onMounted, onUnmounted } from 'vue';
import { Swiper, SwiperSlide } from 'swiper/vue';
import { EffectCoverflow, Autoplay } from 'swiper/modules';
import type { Swiper as SwiperClass } from 'swiper';

import 'swiper/css';
import 'swiper/css/effect-coverflow';

const screenshots = ref<Array<{ src: string; alt: string; path: string }>>([]);
const modules = [EffectCoverflow, Autoplay];
const isMounted = ref(false);
const initialSlideIndex = ref(0);
const preloadedIndexes = new Set<number>();
let swiperInstance: SwiperClass | null = null;

const zoomedItem = ref<{ src: string; alt: string } | null>(null);

let pointerStartX = 0;
let pointerStartY = 0;
let isDragging = false;

function onPointerDown(e: PointerEvent) {
  pointerStartX = e.clientX;
  pointerStartY = e.clientY;
  isDragging = false;
}

function onPointerMove(e: PointerEvent) {
  if (Math.abs(e.clientX - pointerStartX) > 6 || Math.abs(e.clientY - pointerStartY) > 6) {
    isDragging = true;
  }
}

function handleImageClick(e: MouseEvent, item: { src: string; alt: string }) {
  if (isDragging) {
    e.preventDefault();
    e.stopPropagation();
    return;
  }

  const wrapper = e.currentTarget as HTMLElement;
  const slideEl = wrapper.closest('.hero-slide');
  if (!slideEl || !slideEl.classList.contains('swiper-slide-active')) {
    e.preventDefault();
    e.stopPropagation();
    return;
  }

  zoomedItem.value = item;
  if (swiperInstance && swiperInstance.autoplay) {
    swiperInstance.autoplay.stop();
  }
}

function closeZoom() {
  zoomedItem.value = null;
  if (swiperInstance && swiperInstance.autoplay) {
    swiperInstance.autoplay.start();
  }
}

function handleKeyDown(e: KeyboardEvent) {
  if (e.key === 'Escape' || e.key === 'Esc' || e.keyCode === 27) {
    if (zoomedItem.value) {
      closeZoom();
    }
  }
}

function handleScroll() {
  if (zoomedItem.value) {
    closeZoom();
  }
}

function handleVisibilityOrFocusChange() {
  if (!swiperInstance || !swiperInstance.autoplay) return;

  if (document.hidden || !document.hasFocus() || zoomedItem.value) {
    swiperInstance.autoplay.stop();
  } else {
    swiperInstance.autoplay.start();
  }
}

function preloadImage(index: number) {
  if (screenshots.value.length === 0) return;
  const count = screenshots.value.length;
  const normalizedIndex = ((index % count) + count) % count;
  if (preloadedIndexes.has(normalizedIndex)) return;

  preloadedIndexes.add(normalizedIndex);
  const img = new Image();
  img.src = screenshots.value[normalizedIndex].src;
}

function preloadAround(realIndex: number) {
  if (screenshots.value.length === 0) return;

  // Preload current slide, previous slide, and next few slides for smooth playback
  preloadImage(realIndex);
  preloadImage(realIndex + 1);
  preloadImage(realIndex + 2);
  preloadImage(realIndex - 1);
}

function onSwiper(swiper: SwiperClass) {
  swiperInstance = swiper;
  preloadAround(swiper.realIndex);
}

function onSlideChange(swiper: SwiperClass) {
  swiperInstance = swiper;
  preloadAround(swiper.realIndex);
}

onMounted(async () => {
  try {
    const data = await import('../exampleScreenshots.json');
    screenshots.value = data.default || data;
    if (screenshots.value.length > 0) {
      initialSlideIndex.value = Math.floor(Math.random() * screenshots.value.length);
      preloadAround(initialSlideIndex.value);
    }
  } catch (e) {
    screenshots.value = [];
  }
  isMounted.value = true;

  if (typeof window !== 'undefined') {
    document.addEventListener('visibilitychange', handleVisibilityOrFocusChange);
    window.addEventListener('blur', handleVisibilityOrFocusChange);
    window.addEventListener('focus', handleVisibilityOrFocusChange);
    window.addEventListener('keydown', handleKeyDown);
    window.addEventListener('scroll', handleScroll, { passive: true });
  }
});

onUnmounted(() => {
  if (typeof window !== 'undefined') {
    document.removeEventListener('visibilitychange', handleVisibilityOrFocusChange);
    window.removeEventListener('blur', handleVisibilityOrFocusChange);
    window.removeEventListener('focus', handleVisibilityOrFocusChange);
    window.removeEventListener('keydown', handleKeyDown);
    window.removeEventListener('scroll', handleScroll);
  }
});
</script>

<style scoped>
.hero-carousel-container {
  width: 100%;
  max-width: 480px;
  margin: 0 auto;
  padding: 10px 0;
  display: flex;
  justify-content: center;
  align-items: center;
}

.hero-swiper {
  width: 100%;
  padding-top: 20px;
  padding-bottom: 20px;
  overflow: hidden;
}

.hero-slide {
  background-position: center;
  background-size: cover;
  width: 320px;
  max-width: 85vw;
  display: flex;
  justify-content: center;
  align-items: center;
}

.image-wrapper {
  width: 100%;
  height: 260px;
  display: flex;
  justify-content: center;
  align-items: center;
  transition: transform 0.3s ease;
}

.image-wrapper img {
  max-width: 100%;
  max-height: 100%;
  width: auto;
  height: auto;
  object-fit: contain;
  display: block;
  box-shadow: 0 10px 30px rgba(0, 0, 0, 0.25);
  border-radius: 8px;
}

.hero-slide.swiper-slide-active .image-wrapper img {
  cursor: zoom-in;
}

.hero-zoom-overlay {
  position: fixed;
  top: 0;
  right: 0;
  bottom: 0;
  left: 0;
  z-index: 99999;
  background-color: var(--vp-c-bg);
  display: flex;
  justify-content: center;
  align-items: center;
  cursor: zoom-out;
  padding: 20px;
  animation: heroZoomFadeIn 0.25s cubic-bezier(0.2, 0, 0.2, 1);
}

.hero-zoomed-image {
  max-width: 90vw;
  max-height: 90vh;
  object-fit: contain;
  box-shadow: 0 10px 40px rgba(0, 0, 0, 0.35);
  border-radius: 8px;
  cursor: zoom-out;
  animation: heroZoomScaleIn 0.25s cubic-bezier(0.2, 0, 0.2, 1);
}

@keyframes heroZoomFadeIn {
  from {
    opacity: 0;
  }
  to {
    opacity: 1;
  }
}

@keyframes heroZoomScaleIn {
  from {
    transform: scale(0.92);
  }
  to {
    transform: scale(1);
  }
}

@media (min-width: 640px) {
  .hero-carousel-container {
    max-width: 540px;
  }
  .hero-slide {
    width: 360px;
  }
  .image-wrapper {
    height: 290px;
  }
}

@media (min-width: 960px) {
  .hero-carousel-container {
    max-width: 560px;
  }
  .hero-slide {
    width: 380px;
  }
  .image-wrapper {
    height: 310px;
  }
}
</style>
