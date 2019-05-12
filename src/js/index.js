import { Elm } from '../elm/Main.elm'

document.addEventListener('DOMContentLoaded', function (event) {
  // There is currently no way to prevent default for Browser.Events in Elm. I took the pragmatic approach here to just prevent default
  // on the error keys whose default is problematic due to the fact they might scroll the viewport while playing. This should be replaced as
  // soon as we can prevent default for Browser.Events.
  document.addEventListener('keydown', function (e) {
    if (e.key === 'ArrowDown' || e.key === 'ArrowUp' || e.key === 'ArrowLeft' || e.key === 'ArrowRight') {
      e.preventDefault()
    }
  })

  const app = Elm.Main.init({
    node: document.getElementById('elmboy')
  })

  app.ports.setPixelsFromBatches.subscribe(function (elmData) {
    const canvas = document.getElementById(elmData.canvasId)

    setPixelsFromBatches(canvas, elmData.pixelBatches)
  })

  const AudioContext = window.AudioContext|| window.webkitAudioContext
  const audioContext = new AudioContext()
  let lastBufferEnds = 0

  const channelCount = 4

  app.ports.queueAudioSamples.subscribe(function (elmData) {
    if (elmData.length > 0) {
      const buffer = audioContext.createBuffer(channelCount, elmData.length, sampleRate)
      const channel1 = buffer.getChannelData(0)
      const channel2 = buffer.getChannelData(1)
      const channel3 = buffer.getChannelData(2)
      const channel4 = buffer.getChannelData(3)

      const samples = elmData.length / channelCount

      for (let i = 0; i < samples; i++) {
        channel1[i] = elmData[i * channelCount]
        channel2[i] = elmData[i * channelCount + 1]
        channel3[i] = elmData[i * channelCount + 2]
        channel4[i] = elmData[i * channelCount + 3]
      }

      audioContext.destination.channelCount = channelCount;

      const bufferSource = audioContext.createBufferSource()
      bufferSource.buffer = buffer
      bufferSource.connect(audioContext.destination)

      const currentBufferStart = audioContext.currentTime + Math.max(0, lastBufferEnds - audioContext.currentTime)
      lastBufferEnds = currentBufferStart + (elmData.length / channelCount / sampleRate)

      bufferSource.start(currentBufferStart)
    }
  })
})

function setPixelsFromBatches (canvas, pixelBatches) {
  const canvasContext = canvas.getContext('2d')
  const imageData = canvasContext.getImageData(0, 0, screenWidth, screenHeight)

  for (let pixelIndex = 0; pixelIndex < screenWidth * screenHeight; pixelIndex++) {
    const batchIndex = Math.floor(pixelIndex / 16)
    const pixelIndexInBatch = pixelIndex % 16

    const individualPixelMask = 0b11000000000000000000000000000000 >>> (pixelIndexInBatch * 2)
    const shift = 30 - (pixelIndexInBatch * 2)

    const pixelValue = (pixelBatches[batchIndex] & individualPixelMask) >>> shift
    const pixelColor = colorMap[pixelValue]

    imageData.data[pixelIndex * 4] = pixelColor[0]
    imageData.data[pixelIndex * 4 + 1] = pixelColor[1]
    imageData.data[pixelIndex * 4 + 2] = pixelColor[2]
    imageData.data[pixelIndex * 4 + 3] = 255
  }

  canvasContext.putImageData(imageData, 0, 0)
}

const screenWidth = 160

const screenHeight = 144

const colorMap = [
  [155, 188, 15],
  [139, 172, 15],
  [48, 98, 48],
  [15, 56, 15]
]

const sampleRate = 44100
