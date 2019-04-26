using UnityEngine;
using System.Collections;
using System;

namespace Oculus.Platform
{

  public class MicrophoneInput : IMicrophone
  {
    AudioClip microphoneClip;
    int lastMicrophoneSample;
    int micBufferSizeSamples;

    public MicrophoneInput()
    {
      int bufferLenSeconds = 1; //minimum size unity allows
      int inputFreq = 48000; //this frequency is fixed throughout the voip system atm
      microphoneClip = Microphone.Start(null, true, bufferLenSeconds, inputFreq);
      micBufferSizeSamples = bufferLenSeconds * inputFreq;
    }

    public void Start()
    {

    }

    public void Stop()
    {
    }

    public float[] Update()
    {
      int pos = Microphone.GetPosition(null);
      int copySize = 0;
      if (pos < lastMicrophoneSample)
      {
        int endOfBufferSize = micBufferSizeSamples - lastMicrophoneSample;
        copySize = endOfBufferSize + pos;
      }
      else
      {
        copySize = pos - lastMicrophoneSample;
      }

      if (copySize == 0) {
        return null;
      }

      float[] samples = new float[copySize]; //TODO 10376403 - pool this
      microphoneClip.GetData(samples, lastMicrophoneSample);
      lastMicrophoneSample = pos;
      return samples;

    }
  }
}
