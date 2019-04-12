import { Elm } from '../elm/Main.elm'
import nipplejs from 'nipplejs'

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

  console.log('prts', app.ports)

  app.ports.setPixelsFromBatches.subscribe(function (elmData) {
    const canvas = document.getElementById(elmData.canvasId)

    initializeVirtualDPad()

    setPixelsFromBatches(canvas, elmData.pixelBatches)
  })

  let virtualDPad = null
  let lastVirtualDPadDirection = null
  const initializeVirtualDPad = () => {
    if (!virtualDPad) {
      virtualDPad = nipplejs.create({
        zone: document.getElementById('virtual-dpad'),
        color: 'blue'
      });

      virtualDPad.on('dir:up', (t, e) => {
        if (lastVirtualDPadDirection) {
          app.ports.virtualDPadInputUp.send(lastVirtualDPadDirection)
        }

        app.ports.virtualDPadInput.send('up')
        lastVirtualDPadDirection = 'up'
      })

      virtualDPad.on('dir:down', (t, e) => {
        if (lastVirtualDPadDirection) {
          app.ports.virtualDPadInputUp.send(lastVirtualDPadDirection)

        }
        app.ports.virtualDPadInput.send('down')
        lastVirtualDPadDirection = 'down'
      })

      virtualDPad.on('dir:left', (t, e) => {
        if (lastVirtualDPadDirection) {
          app.ports.virtualDPadInputUp.send(lastVirtualDPadDirection)

        }
        app.ports.virtualDPadInput.send('left')
        lastVirtualDPadDirection = 'left'
      })

      virtualDPad.on('dir:right', (t, e) => {
        if (lastVirtualDPadDirection) {
          app.ports.virtualDPadInputUp.send(lastVirtualDPadDirection)

        }
        app.ports.virtualDPadInput.send('right')
        lastVirtualDPadDirection = 'right'
      })

      virtualDPad.on('end', (t, e) => {
        if (lastVirtualDPadDirection) {
          app.ports.virtualDPadInputUp.send(lastVirtualDPadDirection)
        }

        lastVirtualDPadDirection = null
      })
    }
  }


  const AudioContext = window.AudioContext|| window.webkitAudioContext
  const audioContext = new AudioContext()
  let lastBufferEnds = 0

  app.ports.queueAudioSamples.subscribe(function (elmData) {
    if (elmData.length > 0) {
      const buffer = audioContext.createBuffer(2, elmData.length, sampleRate)
      const leftChannel = buffer.getChannelData(0)
      const rightChannel = buffer.getChannelData(1)

      for (let i = 0; i < elmData.length; i++) {
        leftChannel[i] = elmData[elmData.length - 1 - i][0]
        rightChannel[i] = elmData[elmData.length - 1 - i][1]
      }

      var bufferSource = audioContext.createBufferSource()
      bufferSource.buffer = buffer
      bufferSource.connect(audioContext.destination)

      bufferSource.start(lastBufferEnds)
      lastBufferEnds += (elmData.length / sampleRate)
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
