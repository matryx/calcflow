// This is for a future releases. If video capture is at realtime this will record all audio sources and create a file in the capture directiory.


using UnityEngine;
using System.Collections;
using System.IO;
using System;
using LSToolKit;

public class LSAudioCapture : MonoBehaviour {

    int bufferSize;
    int numBuffers;
    int outputRate = 44100;
    public string fileName = "_audio.wav";
    int headerSize = 44;
    bool recOutput;
    FileStream fileStream;
    KeyCode captureKey;

    void Awake()
    {
        AudioSettings.outputSampleRate = outputRate;
    }

    // Use this for initialization
    void Start () {
        AudioSettings.GetDSPBufferSize(out bufferSize, out numBuffers);
        if(LS360VRCamera.Instance() != null)
        {
            captureKey = LS360VRCamera.Instance().captureVideoKey;
        } else
        {
            Debug.LogError("Error: No LS360VRCamera Found");
        }
    }
	
	// Update is called once per frame
	void Update () {

        if(LS360VRCamera.Instance().captureEveryFrame == false)
        {
            return;
        }

        if (Input.GetKeyDown(captureKey))
        {
            
            if (recOutput == false)
            {
                print("Started Audio Recording");
                StartWriting("LSCaptureFiles/" + LS360VRCamera.Instance().GetCaptureBaseName() + fileName);
                recOutput = true;
            }
            else
            {
                recOutput = false;
                WriteHeader();
                print("Stopped Audio Recording");
            }
        }
    }

    void StartWriting(String name)
    {
        fileStream = new FileStream(name, FileMode.Create);
        byte emptyByte = new byte();

        for (int i = 0; i < headerSize; i++) //preparing the header
    {
            fileStream.WriteByte(emptyByte);
        }
    }

    void OnAudioFilterRead(float[] data, int channels)
    {
        if (recOutput)
        {
            ConvertAndWrite(data); //audio data is interlaced
        }
    }

    void ConvertAndWrite(float[] dataSource)
    {
 
        Int16[] intData = new Int16[dataSource.Length];
        //converting in 2 steps : float[] to Int16[], //then Int16[] to Byte[]

        Byte[] bytesData = new Byte[(int)dataSource.Length * 2];
        //bytesData array is twice the size of
        //dataSource array because a float converted in Int16 is 2 bytes.

        int rescaleFactor = 32767; //to convert float to Int16

        for (int i = 0; i < dataSource.Length; i++)
    {
            intData[i] = (Int16)(dataSource[i] * rescaleFactor);
            Byte[] byteArr = new Byte[2];
            byteArr = BitConverter.GetBytes(intData[i]);
            byteArr.CopyTo(bytesData, i * 2);
        }

        fileStream.Write(bytesData, 0, bytesData.Length);
    }

    void WriteHeader()
    {

        fileStream.Seek(0, SeekOrigin.Begin);

        Byte[] riff = System.Text.Encoding.UTF8.GetBytes("RIFF");
        fileStream.Write(riff, 0, 4);

        Byte[] chunkSize = BitConverter.GetBytes(fileStream.Length - 8);
        fileStream.Write(chunkSize, 0, 4);

        Byte[] wave = System.Text.Encoding.UTF8.GetBytes("WAVE");
        fileStream.Write(wave, 0, 4);

        Byte[] fmt = System.Text.Encoding.UTF8.GetBytes("fmt ");
        fileStream.Write(fmt, 0, 4);

        Byte[] subChunk1 = BitConverter.GetBytes(16);
        fileStream.Write(subChunk1, 0, 4);

        UInt16 two = 2;
        UInt16 one = 1;

        Byte[] audioFormat = BitConverter.GetBytes(one);
        fileStream.Write(audioFormat, 0, 2);

        Byte[] numChannels = BitConverter.GetBytes(two);
        fileStream.Write(numChannels, 0, 2);

        Byte[] sampleRate = BitConverter.GetBytes(outputRate);
        fileStream.Write(sampleRate, 0, 4);

        Byte[] byteRate = BitConverter.GetBytes(outputRate * 4);
        // sampleRate * bytesPerSample*number of channels, here 44100*2*2

        fileStream.Write(byteRate, 0, 4);

        UInt16 four = 4;
        Byte[] blockAlign = BitConverter.GetBytes(four);
        fileStream.Write(blockAlign, 0, 2);

        UInt16 sixteen = 16;
        Byte[] bitsPerSample = BitConverter.GetBytes(sixteen);
        fileStream.Write(bitsPerSample, 0, 2);

        Byte[] dataString = System.Text.Encoding.UTF8.GetBytes("data");
        fileStream.Write(dataString, 0, 4);

        Byte[] subChunk2 = BitConverter.GetBytes(fileStream.Length - headerSize);
        fileStream.Write(subChunk2, 0, 4);

        fileStream.Close();
    }
}




