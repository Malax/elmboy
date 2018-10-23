import { Elm } from '../elm/Main.elm'

document.addEventListener('DOMContentLoaded', function (event) {
  const app = Elm.Main.init({
    node: document.getElementById('elmboy')
  })

  app.ports.setPixelsFromBatches.subscribe(function (elmData) {
    const canvas = document.getElementById(elmData.canvasId)

    setPixelsFromBatches(canvas, elmData.pixelBatches)
  })

  app.ports.requestFileData.subscribe(function (fileInputId) {
    const file = document.getElementById(fileInputId).files[0]
    const fileReader = new window.FileReader()

    fileReader.addEventListener('load', function () {
      app.ports.fileData.send(Array.from(new Uint8Array(fileReader.result)))
    }, false)

    if (file) {
      fileReader.readAsArrayBuffer(file)
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

    imageData.data[pixelIndex * 4 + 0] = pixelColor[0]
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
