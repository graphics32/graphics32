<template>
  <div class="hero-carousel-container" v-if="isMounted && screenshots.length > 0">
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
        <div class="image-wrapper">
          <img :src="item.src" :alt="item.alt" />
        </div>
      </SwiperSlide>
    </Swiper>
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

function handleVisibilityOrFocusChange() {
  if (!swiperInstance || !swiperInstance.autoplay) return;

  if (document.hidden || !document.hasFocus()) {
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
  }
});

onUnmounted(() => {
  if (typeof window !== 'undefined') {
    document.removeEventListener('visibilitychange', handleVisibilityOrFocusChange);
    window.removeEventListener('blur', handleVisibilityOrFocusChange);
    window.removeEventListener('focus', handleVisibilityOrFocusChange);
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
