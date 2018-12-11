using LSToolKit.Helpers;
using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Threading;
using UnityEngine;
using Bitmap = System.Drawing.Bitmap;
using DrawingImageFormat = System.Drawing.Imaging.ImageFormat;
using ImageLockMode = System.Drawing.Imaging.ImageLockMode;
using PixelFormat = System.Drawing.Imaging.PixelFormat;
using Process = System.Diagnostics.Process;
using Rectangle = System.Drawing.Rectangle;



namespace LSToolKit
{
    public class LS360VRCamera : MonoBehaviour
    {
        [Header("OVR Override Settings")]
        public bool disableOVR = false;
        [Header("File Name & Save Path")]
        public string captureFileName;
        public string saveImagePath = "";
        [Header("Key Mapping")]
        public KeyCode captureStill3DKey = KeyCode.P;
        public KeyCode captureStillKey = KeyCode.O;
        public KeyCode captureVideoKey = KeyCode.I;
        [Header("Image Settings")]
        public ImageFormat imageFormat = ImageFormat.PNG;
        public int still3DResolutionWidth = 8192;
        public int stillResolutionWidth = 4096;
        public int videoResolutionWidth = 2048;
        int resolutionWidth = 8192;
        public AntiAliasing antiAliasing = AntiAliasing._8;
        public int ssaaFactor = 1;

        [Header("Video Capture Options (ALPHA)")]
        public int frameRate = 30;
        public int maxFramesToRecord = 0;

        [Header("Still Capture Options")]
        public bool fadeDuringCapture = true;
        public float fadeTime = 0.25f;
        public Color fadeColor = new Color(0.0f, 0.0f, 0.0f, 1.0f);

        [Header("Other")]
        public bool enableDebugging = false;
        public Camera targetCamera;

        public enum ImageFormat { PNG, JPEG, BMP };
        public enum AntiAliasing { _1 = 1, _2 = 2, _4 = 4, _8 = 8 };

        GameObject[] camGos;
        Camera cam;
        LSCopyCamImageEffect copyCameraScript;

        ComputeShader convertPanoramaShader;
        ComputeShader convertPanoramaStereoShader;
        ComputeShader textureToBufferShader;
        Material fadeMaterial = null;
        bool captureStereoscopic = false;
        float interpupillaryDistance = 0.0635f; // Average IPD of all subjects in US Army survey in meters
        int numCirclePoints = 128;
        int frameNumberDigits = 6;
        float cpuMillisecondsPerFrame = 1000.0f / 120.0f;
        [HideInInspector]
        public bool captureEveryFrame = false;
        bool capturingEveryFrame = false;
        bool usingGpuTransform = true;
        CubemapFace[] faces;
        int panoramaHeight, cameraWidth, cameraHeight;
        RenderTexture cubemapRenderTexture = null;
        Texture2D forceWaitTexture;
        int convertPanoramaKernelIdx = -1, convertPanoramaYPositiveKernelIdx = -1, convertPanoramaYNegativeKernelIdx = -1, textureToBufferIdx = -1, renderStereoIdx = -1;
        int[] convertPanoramaKernelIdxs;
        byte[] imageFileBytes;
        string videoBaseName = "";
        private int frameNumber;
        const int ResultBufferSlices = 8; // Makes result buffer same pixel count as a cubemap texture
        float hFov = -1.0f, vFov = -1.0f, hFovAdjustDegrees = -1.0f, vFovAdjustDegrees = -1.0f, circleRadius = -1.0f;
        int threadsX = 32, threadsY = 32; // Must match all shaders
        int numCameras;
        const int CamerasPerCirclePoint = 4;
        uint[] cameraPixels = null;
        uint[] resultPixels = null;
        float tanHalfHFov, tanHalfVFov, hFovAdjust, vFovAdjust;
        int overlapTextures;
        bool initializeFailed = true;


        const uint BufferSentinelValue = 1419455993; // Must match value in TextureToBufferShader.compute

        // If any of these change, have to call Reinitialize()
        int lastConfiguredPanoramaWidth, lastConfiguredNumCirclePoints, lastConfiguredSsaaFactor;
        float lastConfiguredInterpupillaryDistance;
        bool lastConfiguredCaptureStereoscopic, lastConfiguredSaveCubemap, lastConfiguredUseGpuTransform;
        AntiAliasing lastConfiguredAntiAliasing = AntiAliasing._1;

        DrawingImageFormat FormatToDrawingFormat(ImageFormat format)
        {
            switch (format)
            {
                case ImageFormat.PNG: return DrawingImageFormat.Png;
                case ImageFormat.JPEG: return DrawingImageFormat.Jpeg;
                case ImageFormat.BMP: return DrawingImageFormat.Bmp;
                default: Debug.Assert(false); return DrawingImageFormat.Png;
            }
        }

        string FormatMimeType(ImageFormat format)
        {
            switch (format)
            {
                case ImageFormat.PNG: return "image/png";
                case ImageFormat.JPEG: return "image/jpeg";
                case ImageFormat.BMP: return "image/bmp";
                default: Debug.Assert(false); return "";
            }
        }

        string FormatToExtension(ImageFormat format)
        {
            switch (format)
            {
                case ImageFormat.PNG: return "png";
                case ImageFormat.JPEG: return "jpg";
                case ImageFormat.BMP: return "bmp";
                default: Debug.Assert(false); return "";
            }
        }

        static LS360VRCamera instance;

        public static LS360VRCamera Instance()
        {
            return instance;
        }

        public string GetCaptureBaseName ()
        {
            return videoBaseName;
        }

        public void Awake()
        {
            if (instance == null)
                instance = this;
            else
                Debug.LogError("More than one LS360VRCamera instance detected.");
        }

        public void Start()
        {
            convertPanoramaShader = Resources.Load<ComputeShader>("ConvertPanoramaShader");
            convertPanoramaStereoShader = Resources.Load<ComputeShader>("ConvertPanoramaStereoShader");
            textureToBufferShader = Resources.Load<ComputeShader>("TextureToBufferShader");
            fadeMaterial = Resources.Load<Material>("Fader");

            saveImagePath = "LSCaptureFiles/" + saveImagePath;

            Reinitialize();
        }

        // A function from [-1, 1] to [0, 1] which has f(0)=1, f(-1)=f(1)=0,
        // is symmetric around the y axis, and is smooth near x=0
        float IpdScaleFunction(float latitudeNormalized)
        {
            // return 1.0f; // Most basic, has discontinuities at poles
            // return 1 - latitudeNormalized * latitudeNormalized;

            // First constant is 1/(1 - 1/e^(-1)), second constant is -e^(-1)/(1 - e^(-1)),
            // necessary to satisfy f(0)=1 and f(-1)=f(1)=0.
            return 1.5819767068693265f * Mathf.Exp(-latitudeNormalized * latitudeNormalized) - 0.5819767068693265f;
        }

        public void OnDestroy()
        {
            Cleanup();
        }

        void Cleanup()
        {
            faces = null;

            Destroy(copyCameraScript);
            Destroy(cam);
            if (camGos != null)
                for (int i = camGos.Length - 1; i >= 0; i--)
                    if (camGos[i] != null)
                        Destroy(camGos[i]);
            camGos = null;

            numCameras = -1;
            hFov = vFov = -1.0f;

            if (cubemapRenderTexture != null)
                Destroy(cubemapRenderTexture);
            cubemapRenderTexture = null;

            convertPanoramaKernelIdx = renderStereoIdx = textureToBufferIdx = -1;
            convertPanoramaKernelIdxs = null;

            resultPixels = cameraPixels = null;

            if (forceWaitTexture != null)
                Destroy(forceWaitTexture);
            forceWaitTexture = new Texture2D(1, 1);
        }

        void Reinitialize()
        {
            try
            {
                ReinitializeBody();
            }
            catch (Exception)
            {
                Cleanup();
                throw;
            }
        }

        void ReinitializeBody()
        {
            Log("Settings changed, calling Reinitialize()");

            initializeFailed = true;

            if (!SystemInfo.supportsComputeShaders)
            {
                Debug.LogWarning("CapturePanorama requires compute shaders. Your system does not support them. " +
                    "On PC, compute shaders require DirectX 11, Windows Vista or later, and a GPU capable of Shader Model 5.0.");
                return;
            }

            lastConfiguredCaptureStereoscopic = captureStereoscopic;
            lastConfiguredPanoramaWidth = resolutionWidth;
            lastConfiguredInterpupillaryDistance = interpupillaryDistance;
            lastConfiguredNumCirclePoints = numCirclePoints;
            lastConfiguredSsaaFactor = ssaaFactor;
            lastConfiguredAntiAliasing = antiAliasing;

            Cleanup();

            faces = new CubemapFace[] {
                CubemapFace.PositiveX, CubemapFace.NegativeX,
                CubemapFace.PositiveY, CubemapFace.NegativeY,
                CubemapFace.PositiveZ, CubemapFace.NegativeZ };

            for (int i = 0; i < faces.Length; i++)
                Debug.Assert((int)faces[i] == i); // Required for ConvertPanoramaShader

            panoramaHeight = resolutionWidth / 2;

            // We have a tower of 3 nested GameObjects. First gets the original camera position,
            // second gets the eye rotation/position relative to it, and third holds the camera with default position/rotation.
            camGos = new GameObject[3];
            for (int i = 0; i < 3; i++)
            {
                camGos[i] = new GameObject("PanoramaCaptureCamera" + i);
                camGos[i].hideFlags = HideFlags.HideAndDontSave;
                if (i > 0) camGos[i].transform.parent = camGos[i - 1].transform;
            }


            camGos[2].AddComponent<Camera>();
            cam = camGos[2].GetComponent<Camera>();
            
            
            cam.enabled = false;
            camGos[2].AddComponent<LSCopyCamImageEffect>();
            copyCameraScript = camGos[2].GetComponent<LSCopyCamImageEffect>();
            copyCameraScript.enabled = false;

            numCameras = faces.Length;
            hFov = vFov = 90.0f;
            if (captureStereoscopic)
            {
                // For stereoscopic rendering, there are a set of points lying on a horizontal circle around the origin.
                // Will have four cameras per circle point, one turned left 45 deg, one turned right 45 deg,
                // one turned up 45 deg, one turned down 45 deg. Allows covering >= 180 deg horizontal and 180 deg vertical.
                // To be able to resolve all rays, we need to choose a large enough horizontal FOV for each camera.

                float maxAngleError = 360.0f / numCirclePoints;

                // Given our ipd adjustment scaling f(x), the IPD range of the top/bottom cameras will be up to f(0.5)
                // of the original IPD. Hence the necessary hFov is given by 2*(pi/2 - acos(f(0.5))), usually in the 90-115 deg range.
                float extraFovForRoundingErrors = 0.001f;
                float hFovTopBottom = 2.0f * (Mathf.PI / 2.0f - Mathf.Acos(IpdScaleFunction(0.5f))) * 360.0f / (2.0f * Mathf.PI);
                hFov = Mathf.Max(90f + maxAngleError, hFovTopBottom) + extraFovForRoundingErrors; // These will usually be similar so just use max for simplicity
                vFov = 90.0f;
                numCameras = 2 + numCirclePoints * CamerasPerCirclePoint; // 2 + for top/bottom
                circleRadius = interpupillaryDistance / 2.0f;
                hFovAdjustDegrees = hFov / 2.0f;
                vFovAdjustDegrees = vFov / 2.0f;
            }

            double ppd90 = resolutionWidth * 90.0 / 360.0;
            // Match PPD at 90 degrees - if it's larger, central 90 degree section should match PPD
            cameraWidth = (int)Math.Ceiling(Math.Tan(hFov * (2.0f * Mathf.PI) / 360.0f / 2.0f) * ppd90 * ssaaFactor);
            cameraHeight = (int)Math.Ceiling(Math.Tan(vFov * (2.0f * Mathf.PI) / 360.0f / 2.0f) * ppd90 * ssaaFactor);

            Log("Number of cameras: " + numCameras);
            Log("Camera dimensions: " + cameraWidth + "x" + cameraHeight);

            cubemapRenderTexture = new RenderTexture(cameraWidth, cameraHeight, /*depth*/24, RenderTextureFormat.ARGB32);
            cubemapRenderTexture.antiAliasing = (int)antiAliasing;
            cubemapRenderTexture.Create();

            if (usingGpuTransform)
            {
                convertPanoramaKernelIdx = convertPanoramaShader.FindKernel("CubeMapToEquirectangular");
                convertPanoramaYPositiveKernelIdx = convertPanoramaShader.FindKernel("CubeMapToEquirectangularPositiveY");
                convertPanoramaYNegativeKernelIdx = convertPanoramaShader.FindKernel("CubeMapToEquirectangularNegativeY");
                convertPanoramaKernelIdxs = new int[] { convertPanoramaKernelIdx, convertPanoramaYPositiveKernelIdx, convertPanoramaYNegativeKernelIdx };
                convertPanoramaShader.SetInt("equirectangularWidth", resolutionWidth);
                convertPanoramaShader.SetInt("equirectangularHeight", panoramaHeight);
                convertPanoramaShader.SetInt("ssaaFactor", ssaaFactor);
                convertPanoramaShader.SetInt("cameraWidth", cameraWidth);
                convertPanoramaShader.SetInt("cameraHeight", cameraHeight);

                int sliceHeight = (panoramaHeight + ResultBufferSlices - 1) / ResultBufferSlices;
                int bitmapWidth = resolutionWidth;
                int bitmapHeight = (captureStereoscopic ? 2 * panoramaHeight : sliceHeight);
                resultPixels = new uint[bitmapWidth * bitmapHeight + 1]; // + 1 for sentinel
            }

            textureToBufferIdx = textureToBufferShader.FindKernel("TextureToBuffer");
            textureToBufferShader.SetInt("width", cameraWidth);
            textureToBufferShader.SetInt("height", cameraHeight);
            textureToBufferShader.SetFloat("gamma", QualitySettings.activeColorSpace == ColorSpace.Linear ? 1.0f / 2.2f : 1.0f);

            renderStereoIdx = convertPanoramaStereoShader.FindKernel("RenderStereo");


            tanHalfHFov = Mathf.Tan(hFov * (2 * Mathf.PI) / 360.0f / 2.0f);
            tanHalfVFov = Mathf.Tan(vFov * (2 * Mathf.PI) / 360.0f / 2.0f);
            hFovAdjust = hFovAdjustDegrees * (2 * Mathf.PI) / 360.0f;
            vFovAdjust = vFovAdjustDegrees * (2 * Mathf.PI) / 360.0f;

            if (captureStereoscopic && usingGpuTransform)
            {
                convertPanoramaStereoShader.SetFloat("tanHalfHFov", tanHalfHFov);
                convertPanoramaStereoShader.SetFloat("tanHalfVFov", tanHalfVFov);
                convertPanoramaStereoShader.SetFloat("hFovAdjust", hFovAdjust);
                convertPanoramaStereoShader.SetFloat("vFovAdjust", vFovAdjust);
                convertPanoramaStereoShader.SetFloat("interpupillaryDistance", interpupillaryDistance);
                convertPanoramaStereoShader.SetFloat("circleRadius", circleRadius);
                convertPanoramaStereoShader.SetInt("numCirclePoints", numCirclePoints);
                convertPanoramaStereoShader.SetInt("equirectangularWidth", resolutionWidth);
                convertPanoramaStereoShader.SetInt("equirectangularHeight", panoramaHeight);
                convertPanoramaStereoShader.SetInt("cameraWidth", cameraWidth);
                convertPanoramaStereoShader.SetInt("cameraHeight", cameraHeight);
                convertPanoramaStereoShader.SetInt("ssaaFactor", ssaaFactor);
            }

            initializeFailed = false;
        }

        void Log(string s)
        {
            if (enableDebugging)
                Debug.Log(s, this);
        }

        public void Update()
        {

            bool captureStillKeyPressed = Input.GetKeyDown(captureStillKey);

            if(captureStillKeyPressed)
            {
                resolutionWidth = stillResolutionWidth;
            }

            bool capture3DKeyPressed = Input.GetKeyDown(captureStill3DKey);

            if (capture3DKeyPressed)
            {
                resolutionWidth = still3DResolutionWidth;
            }

            bool captureVideoKeyPressed = Input.GetKeyDown(captureVideoKey);

            if (captureVideoKeyPressed)
            {
                resolutionWidth = videoResolutionWidth;
            }

            if (initializeFailed || resolutionWidth < 4 || (captureStereoscopic && numCirclePoints < 8)) // Can occur temporarily while modifying properties in editor
            {
                if (captureStillKeyPressed || capture3DKeyPressed || captureVideoKeyPressed)
                {
                    if (!Directory.Exists(saveImagePath))
                    {
                        //if it doesn't, create it
                        Directory.CreateDirectory(saveImagePath);

                    }

                    if (resolutionWidth < 4)
                        Debug.LogError("Panorama Width must be at least 4. No panorama captured.");
                    if (captureStereoscopic && numCirclePoints < 8)
                        Debug.LogError("Num Circle Points must be at least 8. No panorama captured.");
                    if (initializeFailed)
                        Debug.LogError("Initialization of Capture Panorama script failed. Cannot capture content.");
                }
                return;
            }

            if (capture3DKeyPressed)
            {
                if (!Directory.Exists(saveImagePath))
                {
                    //if it doesn't, create it
                    Directory.CreateDirectory(saveImagePath);

                }
                captureStereoscopic = true;
                captureEveryFrame = false;
            }
            else if (captureVideoKeyPressed && capturingEveryFrame == false)
            {
                if (!Directory.Exists(saveImagePath))
                {
                    //if it doesn't, create it
                    Directory.CreateDirectory(saveImagePath);

                }
                captureStereoscopic = false;
                captureEveryFrame = true;
            }
            else if (captureStillKeyPressed && capturingEveryFrame == false)
            {
                if (!Directory.Exists(saveImagePath))
                {
                    //if it doesn't, create it
                    Directory.CreateDirectory(saveImagePath);
                }
                captureStereoscopic = false;
                captureEveryFrame = false;
            }


            if (captureStereoscopic != lastConfiguredCaptureStereoscopic ||
                resolutionWidth != lastConfiguredPanoramaWidth ||
                interpupillaryDistance != lastConfiguredInterpupillaryDistance ||
                numCirclePoints != lastConfiguredNumCirclePoints ||
                ssaaFactor != lastConfiguredSsaaFactor ||
                antiAliasing != lastConfiguredAntiAliasing)
            {
                Log("Reinitialize");
                Reinitialize();
            }

            

            if (capturingEveryFrame)
            {
                if (captureVideoKey != KeyCode.None && captureVideoKeyPressed || (maxFramesToRecord > 0 && frameNumber >= maxFramesToRecord))
                {
                    StopCaptureEveryFrame();
                    captureEveryFrame = false;
                }
                else
                {
                    CaptureScreenshotSync(videoBaseName + "_" + frameNumber.ToString(new String('0', frameNumberDigits)));
                    frameNumber += 1;
                }
            }
            else if ((captureVideoKeyPressed || capture3DKeyPressed || captureStillKeyPressed) && !Capturing)
            {
                if (captureEveryFrame)
                {
                    StartCaptureEveryFrame();
                }
                else
                {
                    string filenameBase = String.Format("{0}_{1:yyyy-MM-dd_HH-mm-ss-fff}", captureFileName, DateTime.Now);
                    Log("Panorama capture key pressed, capturing " + filenameBase);
                    CaptureScreenshotAsync(filenameBase);
                }
            } else if ((captureVideoKeyPressed || capture3DKeyPressed || captureStillKeyPressed) && Capturing)
            {
                Debug.Log("Still Capturing");
            }
        }

        public void StartCaptureEveryFrame()
        {
            StartCoroutine("StartFullCapture");
        }

        IEnumerator StartFullCapture ()
        {
            if(disableOVR)
            {
                UnityEngine.XR.XRSettings.enabled = false;
            }
            
            yield return new WaitForSeconds(0.2f);
            Time.captureFramerate = frameRate;
            videoBaseName = String.Format("{0}_{1:yyyy-MM-dd_HH-mm-ss-fff}", captureFileName, DateTime.Now);
            frameNumber = 0;

            capturingEveryFrame = true;
        }

        public void StopCaptureEveryFrame()
        {
            Time.captureFramerate = 0;
            capturingEveryFrame = false;
            if (disableOVR)
            {
                UnityEngine.XR.XRSettings.enabled = true;
            }
        }

        public void CaptureScreenshotSync(string filenameBase)
        {
            var enumerator = CaptureScreenshotAsyncHelper(filenameBase, /*async*/false);
            while (enumerator.MoveNext()) { }
        }



        public void CaptureScreenshotAsync(string filenameBase)
        {
            StartCoroutine(CaptureScreenshotAsyncHelper(filenameBase, /*async*/true));
        }

        internal bool Capturing;

        static List<Process> resizingProcessList = new List<Process>();
        static List<string> resizingFilenames = new List<string>();

        void SetFadersEnabled(IEnumerable<LSFadeController> fadeControls, bool value)
        {
            foreach (LSFadeController fadeControl in fadeControls)
                fadeControl.enabled = value;
        }

        public IEnumerator FadeOut(IEnumerable<LSFadeController> fadeControls)
        {
            Log("Doing fade out");
            // Derived from OVRScreenFade
            float elapsedTime = 0.0f;
            Color color = fadeColor;
            color.a = 0.0f;
            fadeMaterial.color = color;
            SetFadersEnabled(fadeControls, true);
            while (elapsedTime < fadeTime)
            {
                yield return new WaitForEndOfFrame();
                elapsedTime += Time.deltaTime;
                color.a = Mathf.Clamp01(elapsedTime / fadeTime);
                fadeMaterial.color = color;
            }
            if (disableOVR)
            {
                UnityEngine.XR.XRSettings.enabled = false;
            }
            
        }

        public IEnumerator FadeIn(IEnumerable<LSFadeController> fadeControls)
        {
            if (disableOVR)
            {
                UnityEngine.XR.XRSettings.enabled = true;
            }
            Log("Fading back in");
            float elapsedTime = 0.0f;
            Color color = fadeMaterial.color = fadeColor;
            while (elapsedTime < fadeTime)
            {
                yield return new WaitForEndOfFrame();
                elapsedTime += Time.deltaTime;
                color.a = 1.0f - Mathf.Clamp01(elapsedTime / fadeTime);
                fadeMaterial.color = color;
            }
            SetFadersEnabled(fadeControls, false);
        }


        public IEnumerator CaptureScreenshotAsyncHelper(string filenameBase, bool async)
        {
            if (async)
                while (Capturing)
                    yield return null; // If CaptureScreenshot() was called programmatically multiple times, serialize the coroutines
            Capturing = true;

            if (!OnCaptureStart())
            {
                Capturing = false;
                yield break;
            }

            // Have to refresh cameras each frame during video in case cameras or image effects change - consider an option for this.
            Camera[] cameras = GetCaptureCameras();
            Array.Sort(cameras, (x, y) => x.depth.CompareTo(y.depth));

            if (cameras.Length == 0)
            {
                Debug.LogWarning("No cameras found to capture");
                Capturing = false;
                yield break;
            }

            // Need to do this first in case we need to reinitialize
            if (antiAliasing != AntiAliasing._1)
            {
                foreach (Camera c in cameras)
                {
                    if (c.actualRenderingPath == RenderingPath.DeferredLighting ||
                        c.actualRenderingPath == RenderingPath.DeferredShading)
                    {
                        Debug.LogWarning("CapturePanorama: Setting Anti Aliasing=1 because at least one camera in deferred mode. Use SSAA setting or Antialiasing image effect if needed.");
                        antiAliasing = AntiAliasing._1;
                        Reinitialize();
                        break;
                    }
                }
            }

            Log("Starting panorama capture");

            List<LSFadeController> fadeControls = new List<LSFadeController>();
            foreach (Camera c in Camera.allCameras)
            {
                if (c.isActiveAndEnabled && c.targetTexture == null) // Is a camera visible to the player
                {
                    var fadeControl = c.gameObject.AddComponent<LSFadeController>();
                    fadeControl.fadeSourceMaterial = fadeMaterial;
                    fadeControls.Add(fadeControl);
                }
            }
            SetFadersEnabled(fadeControls, false);

            if (fadeDuringCapture && async)
                yield return StartCoroutine(FadeOut(fadeControls));

            // Make sure black is shown before we start - sometimes two frames are needed
            for (int i = 0; i < 2; i++)
                yield return new WaitForEndOfFrame();

            // Initialize compute buffers - do here instead of in Reinitialize() to work around error on Destroy()
            ComputeBuffer convertPanoramaResultBuffer = null;
            ComputeBuffer forceWaitResultConvertPanoramaStereoBuffer = null;
            if (usingGpuTransform)
            {
                if (captureStereoscopic)
                {
                    convertPanoramaResultBuffer =
                        new ComputeBuffer(/*count*/resolutionWidth * panoramaHeight * 2 + 1, /*stride*/4); // + 1 for sentinel
                    convertPanoramaStereoShader.SetBuffer(renderStereoIdx, "result", convertPanoramaResultBuffer);

                    forceWaitResultConvertPanoramaStereoBuffer = new ComputeBuffer(/*count*/1, /*stride*/4);
                    convertPanoramaStereoShader.SetBuffer(renderStereoIdx, "forceWaitResultBuffer", forceWaitResultConvertPanoramaStereoBuffer);
                }
                else
                {
                    int sliceHeight = (panoramaHeight + ResultBufferSlices - 1) / ResultBufferSlices;
                    convertPanoramaResultBuffer =
                        new ComputeBuffer(/*count*/resolutionWidth * sliceHeight + 1, /*stride*/4); // + 1 for sentinel
                    foreach (int kernelIdx in convertPanoramaKernelIdxs)
                        convertPanoramaShader.SetBuffer(kernelIdx, "result", convertPanoramaResultBuffer);
                }
            }
            int cameraPixelsBufferNumTextures = numCameras;
            overlapTextures = 0;
            int circlePointCircularBufferSize = 0;
            if (captureStereoscopic && usingGpuTransform)
            {
                overlapTextures = ssaaFactor == 1 ? 1 : 2;  // Overlap of 1 supports blending between circle points, overlap of 2 supports it even with SSAA at boundaries
                circlePointCircularBufferSize = 1 + overlapTextures;
                // 2 + for top/bottom, and divide by 2 because we're doing left/right and up/down separately
                cameraPixelsBufferNumTextures = Math.Min(numCameras, 2 + (CamerasPerCirclePoint / 2) * circlePointCircularBufferSize);
            }
            ComputeBuffer cameraPixelsBuffer = new ComputeBuffer(/*count*/cameraPixelsBufferNumTextures * cameraWidth * cameraHeight + 1, /*stride*/4);
            textureToBufferShader.SetBuffer(textureToBufferIdx, "result", cameraPixelsBuffer);

            // Set up sentinels to detect out of graphics memory
            textureToBufferShader.SetInt("sentinelIdx", cameraPixelsBuffer.count - 1);
            if (usingGpuTransform && !captureStereoscopic)
            {
                convertPanoramaShader.SetInt("cameraPixelsSentinelIdx", cameraPixelsBuffer.count - 1);
                convertPanoramaShader.SetInt("sentinelIdx", convertPanoramaResultBuffer.count - 1);
                foreach (int kernelIdx in convertPanoramaKernelIdxs)
                    convertPanoramaShader.SetBuffer(kernelIdx, "cameraPixels", cameraPixelsBuffer);
            }
            if (usingGpuTransform && captureStereoscopic)
            {
                convertPanoramaStereoShader.SetInt("cameraPixelsSentinelIdx", cameraPixelsBuffer.count - 1);
                convertPanoramaStereoShader.SetBuffer(renderStereoIdx, "cameraPixels", cameraPixelsBuffer);
            }

            ComputeBuffer forceWaitResultTextureToBufferBuffer = new ComputeBuffer(/*count*/1, /*stride*/4);
            textureToBufferShader.SetBuffer(textureToBufferIdx, "forceWaitResultBuffer", forceWaitResultTextureToBufferBuffer);

            float startTime = Time.realtimeSinceStartup;

            Quaternion headOrientation = Quaternion.identity;




            Log("Rendering camera views");
            foreach (Camera c in cameras)
                Log("Camera name: " + c.gameObject.name);

            var methodMap = new Dictionary<Camera, List<LSCopyCamImageEffect.InstanceMethodPair>>();
            foreach (Camera c in cameras)
                methodMap[c] = LSCopyCamImageEffect.GenerateMethodList(c);

            // Need to extract each cubemap into a Texture2D so we can read the pixels, but Unity bug
            // prevents this with antiAliasing: http://issuetracker.unity3d.com/issues/texture2d-dot-readpixels-fails-if-rendertexture-has-anti-aliasing-set
            // We copy the cubemap textures using a shader as a workaround.

            string suffix = "." + FormatToExtension(imageFormat);
            string filePath = "";
            // Save in separate thread to avoid hiccups
            string imagePath = saveImagePath;
            if (imagePath == null || imagePath == "")
            {
                imagePath = Application.dataPath + "/..";
            }

            convertPanoramaStereoShader.SetInt("circlePointCircularBufferSize", circlePointCircularBufferSize);
            int nextCirclePointCircularBufferStart = 0, nextCirclePointStart = 0, writeIdx = 0;
            int ilimit = usingGpuTransform ? numCameras + overlapTextures * CamerasPerCirclePoint : numCameras;
            int leftRightPhaseEnd = (ilimit - 2) / 2 + 2;
            int circlePointsRendered = 0;

            BeforeRenderPanorama();

            RenderTexture.active = null;
            for (int i = 0; i < ilimit; i++)
            {
                // Don't use RenderToCubemap - it causes problems with compositing multiple cameras, and requires
                // more temporary VRAM. Just render cube map manually.
                if (captureStereoscopic)
                {
                    if (i < 2)
                    {
                        // 0, 1 are top/bottom caps
                        camGos[1].transform.localPosition = Vector3.zero;
                        camGos[1].transform.localRotation = Quaternion.Euler((i == 0) ? 90.0f : -90.0f, 0.0f, 0.0f);
                    }
                    else
                    {
                        // Do all left/right textures first then all up/down textures
                        int iAdjusted, numInGroupBias;
                        if (i < leftRightPhaseEnd)
                        {
                            iAdjusted = i - 2;
                            numInGroupBias = 0;
                        }
                        else
                        {
                            iAdjusted = i - leftRightPhaseEnd;
                            numInGroupBias = 2;
                        }

                        int circlePointNum = (iAdjusted / (CamerasPerCirclePoint / 2)) % numCirclePoints;
                        int numInGroup = iAdjusted % (CamerasPerCirclePoint / 2) + numInGroupBias;

                        float circleAngle = 360.0f * circlePointNum / numCirclePoints;
                        camGos[1].transform.localPosition = Quaternion.Euler(0.0f, circleAngle, 0.0f) * Vector3.forward * circleRadius;

                        if (numInGroup < 2)
                            camGos[1].transform.localRotation = Quaternion.Euler(0.0f, circleAngle + (numInGroup == 0 ? -hFovAdjustDegrees : hFovAdjustDegrees), 0.0f);
                        else
                            camGos[1].transform.localRotation = Quaternion.Euler((numInGroup == 2 ? -vFovAdjustDegrees : vFovAdjustDegrees), circleAngle, 0.0f);

                        if (numInGroup == 1 || numInGroup == 3) circlePointsRendered++;
                    }
                }
                else
                {
                    switch ((CubemapFace)i)
                    {
                        case CubemapFace.PositiveX: camGos[1].transform.localRotation = Quaternion.Euler(0.0f, 90.0f, 0.0f); break;
                        case CubemapFace.NegativeX: camGos[1].transform.localRotation = Quaternion.Euler(0.0f, -90.0f, 0.0f); break;
                        case CubemapFace.PositiveY: camGos[1].transform.localRotation = Quaternion.Euler(90.0f, 0.0f, 0.0f); break;
                        case CubemapFace.NegativeY: camGos[1].transform.localRotation = Quaternion.Euler(-90.0f, 0.0f, 0.0f); break;
                        case CubemapFace.PositiveZ: camGos[1].transform.localRotation = Quaternion.Euler(0.0f, 0.0f, 0.0f); break;
                        case CubemapFace.NegativeZ: camGos[1].transform.localRotation = Quaternion.Euler(0.0f, 180.0f, 0.0f); break;
                    }
                }
              
                foreach (Camera c in cameras)
                {
                    // To get the camera in the right eye position, migrate the camera transform to camGos[0]
                    camGos[2].transform.parent = null;
                    cam.CopyFrom(c);

                    camGos[0].transform.localPosition = cam.transform.localPosition;
                    camGos[0].transform.localRotation = cam.transform.localRotation;
                    camGos[2].transform.parent = camGos[1].transform;
                    cam.transform.localPosition = Vector3.zero;
                    cam.transform.localRotation = Quaternion.identity;
                    copyCameraScript.enabled = methodMap[c].Count > 0;
                    copyCameraScript.onRenderImageMethods = methodMap[c];
                    cam.fieldOfView = vFov; // hFov inferred from aspect ratio of target

                    camGos[0].transform.rotation *= Quaternion.Inverse(headOrientation);

                    cam.targetTexture = cubemapRenderTexture;

                    // Aspect ratio must be determined by size of render target. This is critical when Unity native VR is enabled.
                    cam.ResetAspect();

                    // Temporarily set original camera to same position/rotation/field of view as
                    // rendering camera during render. If any image effects examine camera
                    // orientation/FOV this will ensure they behave correctly.

                    Vector3 savePosition = c.transform.position;
                    Quaternion saveRotation = c.transform.rotation;
                    float saveFieldOfView = c.fieldOfView;
                    RenderTexture saveRenderTexture = c.targetTexture;

                    c.transform.position = cam.transform.position;
                    c.transform.rotation = cam.transform.rotation;
                    c.fieldOfView = cam.fieldOfView;
                    
                    cam.Render();

                    c.transform.position = savePosition;
                    c.transform.rotation = saveRotation;
                    c.fieldOfView = saveFieldOfView;
                    c.targetTexture = saveRenderTexture;
                }

                // Read one pixel from texture to force render to complete before continuing
                RenderTexture.active = cubemapRenderTexture;
                forceWaitTexture.ReadPixels(new Rect(cameraWidth - 1, cameraHeight - 1, 1, 1), 0, 0);

                int forceWaitValue = 1000000 + i;
                textureToBufferShader.SetInt("forceWaitValue", forceWaitValue);
                textureToBufferShader.SetTexture(textureToBufferIdx, "source", cubemapRenderTexture);
                textureToBufferShader.SetInt("startIdx", writeIdx * cameraWidth * cameraHeight);
                textureToBufferShader.Dispatch(textureToBufferIdx, (cameraWidth + threadsX - 1) / threadsX, (cameraHeight + threadsY - 1) / threadsY, 1);

                uint[] forceWaitResult = new uint[1];
                forceWaitResultTextureToBufferBuffer.GetData(forceWaitResult);
                if (forceWaitResult[0] != forceWaitValue)
                    Debug.LogError("TextureToBufferShader: Unexpected forceWaitResult value " + forceWaitResult[0] + ", should be " + forceWaitValue);

                writeIdx++;
                if (writeIdx >= cameraPixelsBufferNumTextures) writeIdx = 2; // Leave top/bottom in indexes 0/1

                // For stereoscopic GPU transform, interleave capture and rendering to decrease VRAM consumption
                if (captureStereoscopic && usingGpuTransform &&
                    ((i - 2) + 1) % (CamerasPerCirclePoint / 2) == 0 &&
                    (circlePointsRendered - nextCirclePointStart >= circlePointCircularBufferSize || i + 1 == 2 + (ilimit - 2) / 2 || i + 1 == ilimit))
                {
                    forceWaitValue = 2000000 + i;
                    convertPanoramaStereoShader.SetInt("forceWaitValue", forceWaitValue);
                    convertPanoramaStereoShader.SetInt("leftRightPass", i < leftRightPhaseEnd ? 1 : 0);
                    convertPanoramaStereoShader.SetInt("circlePointStart", nextCirclePointStart);
                    convertPanoramaStereoShader.SetInt("circlePointEnd", cameraPixelsBufferNumTextures < numCameras ? circlePointsRendered : circlePointsRendered + 1);
                    convertPanoramaStereoShader.SetInt("circlePointCircularBufferStart", nextCirclePointCircularBufferStart);
                    convertPanoramaStereoShader.Dispatch(renderStereoIdx, (resolutionWidth + threadsX - 1) / threadsX, (panoramaHeight + threadsY - 1) / threadsY, 2);

                    forceWaitResultConvertPanoramaStereoBuffer.GetData(forceWaitResult);
                    if (forceWaitResult[0] != forceWaitValue)
                        Debug.LogError("ConvertPanoramaStereoShader: Unexpected forceWaitResult value " + forceWaitResult[0] + ", should be " + forceWaitValue);

                    if (i + 1 == leftRightPhaseEnd)
                    {
                        nextCirclePointCircularBufferStart = (nextCirclePointCircularBufferStart + circlePointCircularBufferSize) % circlePointCircularBufferSize;
                        nextCirclePointStart = 0;
                        circlePointsRendered = 0;
                    }
                    else
                    {
                        nextCirclePointStart = circlePointsRendered - overlapTextures;
                        nextCirclePointCircularBufferStart = (nextCirclePointCircularBufferStart + circlePointCircularBufferSize - overlapTextures) % circlePointCircularBufferSize;
                    }
                }

                RenderTexture.active = null;
            }

            AfterRenderPanorama();

            RenderTexture.active = null;

            // If this is not here, the fade-in will drop frames.
            for (int i = 0; i < 2; i++)
                yield return new WaitForEndOfFrame();

            if (async && !usingGpuTransform && fadeDuringCapture)
                yield return StartCoroutine(FadeIn(fadeControls));

            
            filePath = imagePath + "/" + filenameBase + suffix;

            bool producedImageSuccess = false;

            {
                // Write pixels directly to .NET Bitmap for saving out
                // Based on https://msdn.microsoft.com/en-us/library/5ey6h79d%28v=vs.110%29.aspx
                Bitmap bitmap = new Bitmap(resolutionWidth, panoramaHeight * (captureStereoscopic ? 2 : 1), PixelFormat.Format32bppArgb);
                var bmpData = bitmap.LockBits(new Rectangle(0, 0, bitmap.Width, bitmap.Height), ImageLockMode.WriteOnly, bitmap.PixelFormat);
                IntPtr ptr = bmpData.Scan0;
                byte[] pixelValues = new byte[Math.Abs(bmpData.Stride) * bitmap.Height];

                // Convert to equirectangular projection - use compute shader for better performance if supported by platform

                if (async)
                    yield return StartCoroutine(CubemapToEquirectangular(cameraPixelsBuffer, cameraPixels, convertPanoramaResultBuffer, cameraWidth, cameraHeight, pixelValues, bmpData.Stride, resolutionWidth, panoramaHeight, ssaaFactor, async));
                else
                {
                    var enumerator = CubemapToEquirectangular(cameraPixelsBuffer, cameraPixels, convertPanoramaResultBuffer, cameraWidth, cameraHeight, pixelValues, bmpData.Stride, resolutionWidth, panoramaHeight, ssaaFactor, async);
                    while (enumerator.MoveNext()) { }
                }

                producedImageSuccess = (pixelValues[3] == 255);

                yield return null;
                System.Runtime.InteropServices.Marshal.Copy(pixelValues, 0, ptr, pixelValues.Length);
                bitmap.UnlockBits(bmpData);
                yield return null;

                Log("Time to take panorama screenshot: " + (Time.realtimeSinceStartup - startTime) + " sec");

                if (producedImageSuccess)
                {
                    var thread = new Thread(() =>
                    {
                        Log("Saving equirectangular image");
                        bitmap.Save(filePath, FormatToDrawingFormat(imageFormat));
                    });
                    thread.Start();
                    while (thread.ThreadState == ThreadState.Running)
                        if (async)
                            yield return null;
                        else
                            Thread.Sleep(0);
                }

                bitmap.Dispose();
            }

            // Release ComputeBuffers - all done with these
            foreach (var buffer in new ComputeBuffer[] {
                convertPanoramaResultBuffer,
                cameraPixelsBuffer,
                forceWaitResultConvertPanoramaStereoBuffer,
                forceWaitResultTextureToBufferBuffer })
                if (buffer != null)
                    buffer.Release();
            convertPanoramaResultBuffer = cameraPixelsBuffer = null;

            if (async && usingGpuTransform && fadeDuringCapture)
                yield return StartCoroutine(FadeIn(fadeControls));

            foreach (LSFadeController fadeControl in fadeControls)
            {
                Destroy(fadeControl);
            }
            fadeControls.Clear();

            if (producedImageSuccess && !captureEveryFrame)
            {
                //Success
                Capturing = false;
            }
            else
            {
                //Failed
                if (!producedImageSuccess)
                {
                    if (Camera.main != null)
                        Debug.LogError("Failed to produce image");
                }
                Capturing = false;
            }
        }

        public virtual bool OnCaptureStart()
        {
            return true;
        }

        public virtual Camera[] GetCaptureCameras()
        {
            Camera[] cameras = Camera.allCameras;

            var finalCameras = new List<Camera>();

            //Added
            if(targetCamera != null)
            {
                Camera cam = targetCamera;
                finalCameras.Add(cam);
                return finalCameras.ToArray();
            }

            Debug.LogError("No Target Camera Set");

            return null;
        }

        public virtual void BeforeRenderPanorama()
        {
            // Do nothing, for overriding only
        }

        public virtual void AfterRenderPanorama()
        {
            // Do nothing, for overriding only
        }

        private static void ReportOutOfGraphicsMemory()
        {
            throw new OutOfMemoryException("Exhausted graphics memory while capturing panorama. " +
                "Lower Panorama Width, increase Num Circle Points for stereoscopic images, disable Anti Aliasing, or disable Stereoscopic Capture.");
        }

        private void SaveCubemapImage(uint[] cameraPixels, string filenameBase, string suffix, string imagePath, int i, int bufferIdx)
        {
            Bitmap bitmap = new Bitmap(cameraWidth, cameraHeight, PixelFormat.Format32bppArgb);
            var bmpData = bitmap.LockBits(new Rectangle(0, 0, bitmap.Width, bitmap.Height), ImageLockMode.WriteOnly, bitmap.PixelFormat);
            IntPtr ptr = bmpData.Scan0;
            byte[] pixelValues = new byte[Math.Abs(bmpData.Stride) * bitmap.Height];
            int stride = bmpData.Stride;
            int height = bmpData.Height;
            int inputIdx = bufferIdx * cameraWidth * cameraHeight;
            for (int y = 0; y < cameraHeight; y++)
            {
                int outputIdx = stride * (height - 1 - y);
                for (int x = 0; x < cameraWidth; x++)
                {
                    uint c = cameraPixels[inputIdx];
                    pixelValues[outputIdx + 0] = (byte)(c & 0xFF);
                    pixelValues[outputIdx + 1] = (byte)((c >> 8) & 0xFF);
                    pixelValues[outputIdx + 2] = (byte)(c >> 16);
                    pixelValues[outputIdx + 3] = 255;
                    outputIdx += 4;
                    inputIdx++;
                }
            }
            System.Runtime.InteropServices.Marshal.Copy(pixelValues, 0, ptr, pixelValues.Length);
            bitmap.UnlockBits(bmpData);

            string cameraId;
            if (captureStereoscopic)
            {
                cameraId = i.ToString();
                Log("Saving lightfield camera image number " + cameraId);
            }
            else
            {
                cameraId = ((CubemapFace)i).ToString();
                Log("Saving cubemap image " + cameraId);
            }
            string cubeFilepath = imagePath + "/" + filenameBase + "_" + cameraId + suffix;
            bitmap.Save(cubeFilepath, FormatToDrawingFormat(imageFormat));
            bitmap.Dispose();
        }

        private Color32 GetCameraPixelBilinear(uint[] cameraPixels, int cameraNum, float u, float v)
        {
            u *= cameraWidth;
            v *= cameraHeight;
            int left = (int)Math.Floor(u);
            int right = (int)Math.Min(cameraWidth - 1, left + 1);
            int top = (int)Math.Floor(v);
            int bottom = (int)Math.Min(cameraHeight - 1, top + 1);
            float uFrac = u - left;
            float vFrac = v - top;

            int baseIdx = cameraNum * cameraWidth * cameraHeight;
            int topRow = baseIdx + top * cameraWidth;
            int bottomRow = baseIdx + bottom * cameraWidth;
            uint topLeft = cameraPixels[topRow + left];
            uint topRight = cameraPixels[topRow + right];
            uint bottomLeft = cameraPixels[bottomRow + left];
            uint bottomRight = cameraPixels[bottomRow + right];

            float r = Mathf.Lerp(Mathf.Lerp(topLeft >> 16, bottomLeft >> 16, vFrac),
                                 Mathf.Lerp(topRight >> 16, bottomRight >> 16, vFrac), uFrac);
            float g = Mathf.Lerp(Mathf.Lerp((topLeft >> 8) & 0xFF, (bottomLeft >> 8) & 0xFF, vFrac),
                                 Mathf.Lerp((topRight >> 8) & 0xFF, (bottomRight >> 8) & 0xFF, vFrac), uFrac);
            float b = Mathf.Lerp(Mathf.Lerp(topLeft & 0xFF, bottomLeft & 0xFF, vFrac),
                                 Mathf.Lerp(topRight & 0xFF, bottomRight & 0xFF, vFrac), uFrac);

            return new Color(r / 255.0f, g / 255.0f, b / 255.0f, 1.0f);
        }

        internal void ClearProcessQueue()
        {
            while (resizingProcessList.Count > 0)
            {
                resizingProcessList[0].WaitForExit();
                File.Delete(resizingFilenames[0]);
                resizingProcessList.RemoveAt(0);
                resizingFilenames.RemoveAt(0);
            }
        }
        

        IEnumerator CubemapToEquirectangular(ComputeBuffer cameraPixelsBuffer, uint[] cameraPixels, ComputeBuffer convertPanoramaResultBuffer, int cameraWidth, int cameraHeight, byte[] pixelValues,
            int stride, int panoramaWidth, int panoramaHeight, int ssaaFactor, bool async)
        {
            if (captureStereoscopic && usingGpuTransform)
            {
                convertPanoramaResultBuffer.GetData(resultPixels);
                if (resultPixels[convertPanoramaResultBuffer.count - 1] != BufferSentinelValue)
                    ReportOutOfGraphicsMemory();

                writeOutputPixels(pixelValues, stride, panoramaWidth, panoramaHeight * 2, panoramaHeight * 2, /*yStart*/0);
            }
            else if (captureStereoscopic && !usingGpuTransform)
            {
                float startTime = Time.realtimeSinceStartup;
                float processingTimePerFrame = cpuMillisecondsPerFrame / 1000.0f;

                for (int y = 0; y < panoramaHeight; y++)
                    for (int x = 0; x < panoramaWidth; x++)
                    {
                        float xcoord = (float)x / panoramaWidth;
                        float ycoord = (float)y / panoramaHeight;

                        float latitude = (ycoord - 0.5f) * Mathf.PI;
                        float sinLat = Mathf.Sin(latitude);
                        float cosLat = Mathf.Cos(latitude);
                        float longitude = (xcoord * 2.0f - 1.0f) * Mathf.PI;
                        float sinLong = Mathf.Sin(longitude);
                        float cosLong = Mathf.Cos(longitude);

                        // Scale IPD down as latitude moves toward poles to avoid discontinuities
                        float latitudeNormalized = latitude / (Mathf.PI / 2.0f); // Map to [-1, 1]
                        float ipdScale = IpdScaleFunction(latitudeNormalized);
                        float scaledEyeRadius = ipdScale * interpupillaryDistance / 2.0f;

                        int cameraNum;
                        float u, v;

                        float ipdScaleLerp = 1.0f - ipdScale * 5.0f; // Scale [0, 0.2] to [0, 1] and reverse
                                                                     // Top/bottom cap
                        Color colorCap = new Color(0.0f, 0.0f, 0.0f, 0.0f);
                        if (ipdScaleLerp > 0.0f)
                        {
                            Vector3 equirectRayDirection = new Vector3(cosLat * sinLong, sinLat, cosLat * cosLong);
                            float distance = 1.0f / equirectRayDirection.y;
                            u = equirectRayDirection.x * distance; v = equirectRayDirection.z * distance;
                            if (u * u <= 1 && v * v <= 1)
                            {
                                if (equirectRayDirection.y > 0.0f)
                                {
                                    cameraNum = 0;
                                }
                                else
                                {
                                    u = -u;
                                    cameraNum = 1;
                                }

                                u = (u + 1.0f) * 0.5f;
                                v = (v + 1.0f) * 0.5f;

                                colorCap = GetCameraPixelBilinear(cameraPixels, cameraNum, u, v);
                            }
                        }

                        for (int i = 0; i < 2; i++)
                        {
                            // The following is equivalent to:
                            // Quaternion eyesRotation = Quaternion.Euler(0.0f, longitude * 360.0f / (2 * Mathf.PI), 0.0f);
                            // Vector3 initialEyePosition = (i == 0 ? Vector3.left : Vector3.right) * scaledEyeRadius;
                            // Vector3 pos = eyesRotation * initialEyePosition; // eye position
                            // Vector3 dir = eyesRotation * Vector3.forward; // gaze direction

                            Vector3 dir = new Vector3(sinLong, 0.0f, cosLong);

                            float angle = (Mathf.PI / 2.0f - Mathf.Acos(scaledEyeRadius / circleRadius));
                            if (i == 0) angle = -angle;
                            float circlePointAngle = longitude + angle;
                            if (circlePointAngle < 0.0f) circlePointAngle += 2 * Mathf.PI;
                            if (circlePointAngle >= 2 * Mathf.PI) circlePointAngle -= 2 * Mathf.PI;

                            float circlePointNumber = circlePointAngle / (2 * Mathf.PI) * numCirclePoints;
                            int circlePoint0 = (int)Mathf.Floor(circlePointNumber) % numCirclePoints;

                            // Get color from each adjacent circle point
                            Color color0 = new Color(), color1 = new Color();
                            for (int j = 0; j < 2; j++)
                            {
                                int circlePointIdx = (j == 0 ? circlePoint0 : (circlePoint0 + 1) % numCirclePoints);
                                float cameraPointAngle = 2 * Mathf.PI * circlePointIdx / numCirclePoints;
                                float sinCameraPointAngle = Mathf.Sin(cameraPointAngle);
                                float cosCameraPointAngle = Mathf.Cos(cameraPointAngle);

                                // Equivalent to (using fact that both dir and circlePointNorm are unit vectors):
                                // Quaternion circlePointRotation = Quaternion.Euler(0.0f, cameraPointAngle * 360.0f / (2 * Mathf.PI), 0.0f);
                                // Vector3 circlePointNormal = circlePointRotation * Vector3.forward;
                                // float newLongitude = Mathf.Sign(Vector3.Cross(circlePointNormal, dir).y) * Vector3.Angle(circlePointNormal, dir) * (2 * Mathf.PI) / 360.0f;

                                float newLongitude = Mathf.Sign(dir.x * cosCameraPointAngle - dir.z * sinCameraPointAngle) *
                                                     Mathf.Acos(dir.z * cosCameraPointAngle + dir.x * sinCameraPointAngle);
                                float cosNewLong = Mathf.Cos(newLongitude);
                                float sinNewLong = Mathf.Sin(newLongitude);

                                // Select which of the two cameras for this point to use and adjust ray to make camera plane perpendicular to axes
                                cameraNum = 2 + circlePointIdx * (CamerasPerCirclePoint / 2) + (newLongitude >= 0.0f ? 1 : 0);

                                float longitudeAdjust = (newLongitude >= 0.0f ? -hFovAdjust : hFovAdjust);
                                float longSum = newLongitude + longitudeAdjust;

                                // Equivalent to:
                                // Vector3 textureRayDir = Quaternion.Euler(-latitude * 360.0f / (2 * Mathf.PI), newLongitude * 360.0f / (2 * Mathf.PI), 0.0f) * Vector3.forward;
                                // Vector3 textureRayDirAdjusted = Quaternion.Euler(0.0f, longitudeAdjust * 360.0f / (2 * Mathf.PI), 0.0f) * textureRayDir;
                                Vector3 textureRayDirAdjusted = new Vector3(cosLat * Mathf.Sin(longSum), sinLat, cosLat * Mathf.Cos(longSum));

                                u = textureRayDirAdjusted.x / textureRayDirAdjusted.z / tanHalfHFov;
                                v = -textureRayDirAdjusted.y / textureRayDirAdjusted.z / tanHalfVFov;

                                // There's a lot of vertical overlap so don't accept v near the edge of the left/right cameras, to avoid artifact pixels
                                if (!(textureRayDirAdjusted.z > 0.0f && u * u <= 1.0f && v * v <= 1.0f - 0.1f))
                                {
                                    cameraNum = 2 + numCirclePoints * (CamerasPerCirclePoint / 2) + circlePointIdx * (CamerasPerCirclePoint / 2) + (latitude >= 0.0f ? 1 : 0);
                                    float latitudeAdjust = (latitude >= 0.0f ? vFovAdjust : -vFovAdjust);
                                    float cosLatAdjust = Mathf.Cos(latitudeAdjust);
                                    float sinLatAdjust = Mathf.Sin(latitudeAdjust);
                                    // Equivalent to:
                                    // textureRayDirAdjusted = Quaternion.Euler(latitudeAdjust * 360.0f / (2 * Mathf.PI), 0.0f, 0.0f) * textureRayDir;
                                    textureRayDirAdjusted = new Vector3(cosLat * sinNewLong,
                                                                        cosLatAdjust * sinLat - cosLat * cosNewLong * sinLatAdjust,
                                                                        sinLatAdjust * sinLat + cosLat * cosNewLong * cosLatAdjust);

                                    u = textureRayDirAdjusted.x / textureRayDirAdjusted.z / tanHalfHFov;
                                    v = -textureRayDirAdjusted.y / textureRayDirAdjusted.z / tanHalfVFov;

                                    // Debug.Assert(ipdScaleLerp >= 1.0 || (textureRayDirAdjusted.z > 0.0f && u * u <= 1.0f && v * v <= 1.0f));
                                }

                                u = (u + 1.0f) * 0.5f;
                                v = (v + 1.0f) * 0.5f;

                                Color col = GetCameraPixelBilinear(cameraPixels, cameraNum, u, v);
                                if (j == 0) color0 = col; else color1 = col;
                            }

                            Color32 c = Color.Lerp(color0, color1, circlePointNumber - Mathf.Floor(circlePointNumber));
                            if (colorCap.a > 0.0f && ipdScaleLerp > 0.0f)
                                c = Color.Lerp(c, colorCap, ipdScaleLerp);

                            int outputIdx = stride * (y + panoramaHeight * i) + x * 4;
                            pixelValues[outputIdx + 0] = c.b;
                            pixelValues[outputIdx + 1] = c.g;
                            pixelValues[outputIdx + 2] = c.r;
                            pixelValues[outputIdx + 3] = 255;
                        }

                        if ((x & 0xFF) == 0 && Time.realtimeSinceStartup - startTime > processingTimePerFrame)
                        {
                            yield return null; // Wait until next frame
                            startTime = Time.realtimeSinceStartup;
                        }
                    }
            }
            else if (!captureStereoscopic && usingGpuTransform)
            {
                int sliceHeight = (panoramaHeight + ResultBufferSlices - 1) / ResultBufferSlices;

                Log("Invoking GPU shader for equirectangular reprojection");
                int endYNegative = (int)Mathf.Floor(panoramaHeight * 0.25f);
                int startYPositive = (int)Mathf.Ceil(panoramaHeight * 0.75f);
                for (int sliceNum = 0; sliceNum < ResultBufferSlices; sliceNum++)
                {
                    int startSlice = sliceNum * sliceHeight;
                    int endSlice = Math.Min(startSlice + sliceHeight, panoramaHeight);
                    convertPanoramaShader.SetInt("startY", sliceNum * sliceHeight);
                    convertPanoramaShader.SetInt("sliceHeight", endSlice - startSlice);
                    if (endSlice <= endYNegative)
                        convertPanoramaShader.Dispatch(convertPanoramaYNegativeKernelIdx, (panoramaWidth + threadsX - 1) / threadsX, (sliceHeight + threadsY - 1) / threadsY, 1);
                    else if (startSlice >= startYPositive)
                        convertPanoramaShader.Dispatch(convertPanoramaYPositiveKernelIdx, (panoramaWidth + threadsX - 1) / threadsX, (sliceHeight + threadsY - 1) / threadsY, 1);
                    else
                        convertPanoramaShader.Dispatch(convertPanoramaKernelIdx, (panoramaWidth + threadsX - 1) / threadsX, (panoramaHeight + threadsY - 1) / threadsY, 1);

                    convertPanoramaResultBuffer.GetData(resultPixels);
                    if (resultPixels[convertPanoramaResultBuffer.count - 1] != BufferSentinelValue)
                        ReportOutOfGraphicsMemory();

                    writeOutputPixels(pixelValues, stride, panoramaWidth, sliceHeight, panoramaHeight, startSlice);
                }
            }
            else
            {
                if (async)
                    yield return StartCoroutine(CubemapToEquirectangularCpu(cameraPixels, cameraWidth, cameraHeight, pixelValues,
                        stride, panoramaWidth, panoramaHeight, ssaaFactor, async));
                else
                {
                    var enumerator = CubemapToEquirectangularCpu(cameraPixels, cameraWidth, cameraHeight, pixelValues,
                        stride, panoramaWidth, panoramaHeight, ssaaFactor, async);
                    while (enumerator.MoveNext()) { }
                }
            }
        }

        private void writeOutputPixels(byte[] pixelValues, int stride, int bitmapWidth, int inHeight, int outHeight, int yStart)
        {
            int inputIdx = 0;
            for (int y = yStart; y < yStart + inHeight && y < outHeight; y++)
            {
                int outputIdx = stride * y;
                for (int x = 0; x < bitmapWidth; x++)
                {
                    uint packedCol = resultPixels[inputIdx];
                    pixelValues[outputIdx + 0] = (byte)((packedCol >> 0) & 0xFF);
                    pixelValues[outputIdx + 1] = (byte)((packedCol >> 8) & 0xFF);
                    pixelValues[outputIdx + 2] = (byte)((packedCol >> 16) & 0xFF);
                    pixelValues[outputIdx + 3] = 255;
                    outputIdx += 4;
                    inputIdx++;
                }
            }
        }

        IEnumerator CubemapToEquirectangularCpu(uint[] cameraPixels, int cameraWidth, int cameraHeight, byte[] pixelValues,
            int stride, int panoramaWidth, int panoramaHeight, int ssaaFactor, bool async)
        {
            Log("Converting to equirectangular");

            yield return null; // Wait for next frame at beginning - already used up some time capturing snapshot

            float startTime = Time.realtimeSinceStartup;
            float processingTimePerFrame = cpuMillisecondsPerFrame / 1000.0f;
            float maxWidth = 1.0f - 1.0f / cameraWidth;
            float maxHeight = 1.0f - 1.0f / cameraHeight;
            int numPixelsAveraged = ssaaFactor * ssaaFactor;

            // For efficiency we're going to do a series of rectangles each drawn from only one texture,
            // only using the slow general-case reprojection where necessary.

            int endYPositive = (int)Mathf.Floor(panoramaHeight * 0.25f);
            int startYNegative = (int)Mathf.Ceil(panoramaHeight * 0.75f);

            // 0.195913f is angle in radians between (1, 0, 1) and (1, 1, 1) over pi
            int endTopMixedRegion = (int)Mathf.Ceil(panoramaHeight * (0.5f - 0.195913f));
            int startBottomMixedRegion = (int)Mathf.Floor(panoramaHeight * (0.5f + 0.195913f));

            int startXNegative = (int)Mathf.Ceil(panoramaWidth * 1.0f / 8.0f);
            int endXNegative = (int)Mathf.Floor(panoramaWidth * 3.0f / 8.0f);

            int startZPositive = (int)Mathf.Ceil(panoramaWidth * 3.0f / 8.0f);
            int endZPositive = (int)Mathf.Floor(panoramaWidth * 5.0f / 8.0f);

            int startXPositive = (int)Mathf.Ceil(panoramaWidth * 5.0f / 8.0f);
            int endXPositive = (int)Mathf.Floor(panoramaWidth * 7.0f / 8.0f);

            int startZNegative = (int)Mathf.Ceil(panoramaWidth * 7.0f / 8.0f);
            int endZNegative = (int)Mathf.Floor(panoramaWidth * 1.0f / 8.0f); // z negative wraps/loops around

            if (async)
            {
                yield return StartCoroutine(CubemapToEquirectangularCpuPositiveY(cameraPixels, pixelValues, stride, panoramaWidth, panoramaHeight, ssaaFactor, startTime, processingTimePerFrame, numPixelsAveraged,
                    0, 0, panoramaWidth, endYPositive));
                yield return StartCoroutine(CubemapToEquirectangularCpuNegativeY(cameraPixels, pixelValues, stride, panoramaWidth, panoramaHeight, ssaaFactor, startTime, processingTimePerFrame, numPixelsAveraged,
                    0, startYNegative, panoramaWidth, panoramaHeight));

                yield return StartCoroutine(CubemapToEquirectangularCpuPositiveX(cameraPixels, pixelValues, stride, panoramaWidth, panoramaHeight, ssaaFactor, startTime, processingTimePerFrame, numPixelsAveraged,
                    startXPositive, endTopMixedRegion, endXPositive, startBottomMixedRegion));
                yield return StartCoroutine(CubemapToEquirectangularCpuNegativeX(cameraPixels, pixelValues, stride, panoramaWidth, panoramaHeight, ssaaFactor, startTime, processingTimePerFrame, numPixelsAveraged,
                    startXNegative, endTopMixedRegion, endXNegative, startBottomMixedRegion));
                yield return StartCoroutine(CubemapToEquirectangularCpuPositiveZ(cameraPixels, pixelValues, stride, panoramaWidth, panoramaHeight, ssaaFactor, startTime, processingTimePerFrame, numPixelsAveraged,
                    startZPositive, endTopMixedRegion, endZPositive, startBottomMixedRegion));

                // Do in two pieces since z negative wraps/loops around
                yield return StartCoroutine(CubemapToEquirectangularCpuNegativeZ(cameraPixels, pixelValues, stride, panoramaWidth, panoramaHeight, ssaaFactor, startTime, processingTimePerFrame, numPixelsAveraged,
                    startZNegative, endTopMixedRegion, panoramaWidth, startBottomMixedRegion));
                yield return StartCoroutine(CubemapToEquirectangularCpuNegativeZ(cameraPixels, pixelValues, stride, panoramaWidth, panoramaHeight, ssaaFactor, startTime, processingTimePerFrame, numPixelsAveraged,
                    0, endTopMixedRegion, endZNegative, startBottomMixedRegion));

                // Handle all remaining image areas with the general case
                yield return StartCoroutine(CubemapToEquirectangularCpuGeneralCase(cameraPixels, pixelValues, stride, panoramaWidth, panoramaHeight, ssaaFactor, startTime, processingTimePerFrame, maxWidth, maxHeight, numPixelsAveraged,
                    0, endYPositive, panoramaWidth, endTopMixedRegion));
                yield return StartCoroutine(CubemapToEquirectangularCpuGeneralCase(cameraPixels, pixelValues, stride, panoramaWidth, panoramaHeight, ssaaFactor, startTime, processingTimePerFrame, maxWidth, maxHeight, numPixelsAveraged,
                    0, startBottomMixedRegion, panoramaWidth, startYNegative));

                // If width is not multiple of 8, due to rounding, there may be one-column strips where the X/Z textures mix together
                if (endZNegative < startXNegative)
                    yield return StartCoroutine(CubemapToEquirectangularCpuGeneralCase(cameraPixels, pixelValues, stride, panoramaWidth, panoramaHeight, ssaaFactor, startTime, processingTimePerFrame, maxWidth, maxHeight, numPixelsAveraged,
                        endZNegative, endTopMixedRegion, startXNegative, startBottomMixedRegion));
                if (endXNegative < startZPositive)
                    yield return StartCoroutine(CubemapToEquirectangularCpuGeneralCase(cameraPixels, pixelValues, stride, panoramaWidth, panoramaHeight, ssaaFactor, startTime, processingTimePerFrame, maxWidth, maxHeight, numPixelsAveraged,
                        endXNegative, endTopMixedRegion, startZPositive, startBottomMixedRegion));
                if (endZPositive < startXPositive)
                    yield return StartCoroutine(CubemapToEquirectangularCpuGeneralCase(cameraPixels, pixelValues, stride, panoramaWidth, panoramaHeight, ssaaFactor, startTime, processingTimePerFrame, maxWidth, maxHeight, numPixelsAveraged,
                        endZPositive, endTopMixedRegion, startXPositive, startBottomMixedRegion));
                if (endXPositive < startZNegative)
                    yield return StartCoroutine(CubemapToEquirectangularCpuGeneralCase(cameraPixels, pixelValues, stride, panoramaWidth, panoramaHeight, ssaaFactor, startTime, processingTimePerFrame, maxWidth, maxHeight, numPixelsAveraged,
                        endXPositive, endTopMixedRegion, startZNegative, startBottomMixedRegion));
            }
            else
            {
                IEnumerator enumerator;
                enumerator = CubemapToEquirectangularCpuPositiveY(cameraPixels, pixelValues, stride, panoramaWidth, panoramaHeight, ssaaFactor, startTime, processingTimePerFrame, numPixelsAveraged,
                    0, 0, panoramaWidth, endYPositive);
                while (enumerator.MoveNext()) { }
                enumerator = CubemapToEquirectangularCpuNegativeY(cameraPixels, pixelValues, stride, panoramaWidth, panoramaHeight, ssaaFactor, startTime, processingTimePerFrame, numPixelsAveraged,
                    0, startYNegative, panoramaWidth, panoramaHeight);
                while (enumerator.MoveNext()) { }

                enumerator = CubemapToEquirectangularCpuPositiveX(cameraPixels, pixelValues, stride, panoramaWidth, panoramaHeight, ssaaFactor, startTime, processingTimePerFrame, numPixelsAveraged,
                    startXPositive, endTopMixedRegion, endXPositive, startBottomMixedRegion);
                while (enumerator.MoveNext()) { }
                enumerator = CubemapToEquirectangularCpuNegativeX(cameraPixels, pixelValues, stride, panoramaWidth, panoramaHeight, ssaaFactor, startTime, processingTimePerFrame, numPixelsAveraged,
                    startXNegative, endTopMixedRegion, endXNegative, startBottomMixedRegion);
                while (enumerator.MoveNext()) { }
                enumerator = CubemapToEquirectangularCpuPositiveZ(cameraPixels, pixelValues, stride, panoramaWidth, panoramaHeight, ssaaFactor, startTime, processingTimePerFrame, numPixelsAveraged,
                    startZPositive, endTopMixedRegion, endZPositive, startBottomMixedRegion);
                while (enumerator.MoveNext()) { }

                // Do in two pieces since z negative wraps/loops around
                enumerator = CubemapToEquirectangularCpuNegativeZ(cameraPixels, pixelValues, stride, panoramaWidth, panoramaHeight, ssaaFactor, startTime, processingTimePerFrame, numPixelsAveraged,
                    startZNegative, endTopMixedRegion, panoramaWidth, startBottomMixedRegion);
                while (enumerator.MoveNext()) { }
                enumerator = CubemapToEquirectangularCpuNegativeZ(cameraPixels, pixelValues, stride, panoramaWidth, panoramaHeight, ssaaFactor, startTime, processingTimePerFrame, numPixelsAveraged,
                    0, endTopMixedRegion, endZNegative, startBottomMixedRegion);
                while (enumerator.MoveNext()) { }

                // Handle all remaining image areas with the general case
                enumerator = CubemapToEquirectangularCpuGeneralCase(cameraPixels, pixelValues, stride, panoramaWidth, panoramaHeight, ssaaFactor, startTime, processingTimePerFrame, maxWidth, maxHeight, numPixelsAveraged,
                    0, endYPositive, panoramaWidth, endTopMixedRegion);
                while (enumerator.MoveNext()) { }
                enumerator = CubemapToEquirectangularCpuGeneralCase(cameraPixels, pixelValues, stride, panoramaWidth, panoramaHeight, ssaaFactor, startTime, processingTimePerFrame, maxWidth, maxHeight, numPixelsAveraged,
                    0, startBottomMixedRegion, panoramaWidth, startYNegative);
                while (enumerator.MoveNext()) { }

                // If width is not multiple of 8, due to rounding, there may be one-column strips where the X/Z textures mix together
                if (endZNegative < startXNegative)
                {
                    enumerator = CubemapToEquirectangularCpuGeneralCase(cameraPixels, pixelValues, stride, panoramaWidth, panoramaHeight, ssaaFactor, startTime, processingTimePerFrame, maxWidth, maxHeight, numPixelsAveraged,
                        endZNegative, endTopMixedRegion, startXNegative, startBottomMixedRegion);
                    while (enumerator.MoveNext()) { }
                }
                if (endXNegative < startZPositive)
                {
                    enumerator = CubemapToEquirectangularCpuGeneralCase(cameraPixels, pixelValues, stride, panoramaWidth, panoramaHeight, ssaaFactor, startTime, processingTimePerFrame, maxWidth, maxHeight, numPixelsAveraged,
                        endXNegative, endTopMixedRegion, startZPositive, startBottomMixedRegion);
                    while (enumerator.MoveNext()) { }
                }
                if (endZPositive < startXPositive)
                {
                    enumerator = CubemapToEquirectangularCpuGeneralCase(cameraPixels, pixelValues, stride, panoramaWidth, panoramaHeight, ssaaFactor, startTime, processingTimePerFrame, maxWidth, maxHeight, numPixelsAveraged,
                        endZPositive, endTopMixedRegion, startXPositive, startBottomMixedRegion);
                    while (enumerator.MoveNext()) { }
                }
                if (endXPositive < startZNegative)
                {
                    enumerator = CubemapToEquirectangularCpuGeneralCase(cameraPixels, pixelValues, stride, panoramaWidth, panoramaHeight, ssaaFactor, startTime, processingTimePerFrame, maxWidth, maxHeight, numPixelsAveraged,
                        endXPositive, endTopMixedRegion, startZNegative, startBottomMixedRegion);
                    while (enumerator.MoveNext()) { }
                }
            }

            yield return null;
        }

        private IEnumerator CubemapToEquirectangularCpuPositiveY(uint[] cameraPixels, byte[] pixelValues, int stride, int panoramaWidth, int panoramaHeight, int ssaaFactor, float startTime, float processingTimePerFrame, int numPixelsAveraged,
            int startX, int startY, int endX, int endY)
        {
            for (int y = startY; y < endY; y++)
                for (int x = startX; x < endX; x++)
                {
                    int rTotal = 0, gTotal = 0, bTotal = 0, aTotal = 0;
                    for (int ySsaa = y * ssaaFactor; ySsaa < (y + 1) * ssaaFactor; ySsaa++)
                        for (int xSsaa = x * ssaaFactor; xSsaa < (x + 1) * ssaaFactor; xSsaa++)
                        {
                            float xcoord = (float)xSsaa / (panoramaWidth * ssaaFactor);
                            float ycoord = (float)ySsaa / (panoramaHeight * ssaaFactor);

                            float latitude = (ycoord - 0.5f) * Mathf.PI;
                            float longitude = (xcoord * 2.0f - 1.0f) * Mathf.PI;

                            float cosLat = Mathf.Cos(latitude);
                            Vector3 equirectRayDirection = new Vector3(
                                cosLat * Mathf.Sin(longitude), -Mathf.Sin(latitude), cosLat * Mathf.Cos(longitude));

                            float distance = 1.0f / equirectRayDirection.y;
                            float u = equirectRayDirection.x * distance, v = equirectRayDirection.z * distance;

                            u = (u + 1.0f) / 2.0f;
                            v = (v + 1.0f) / 2.0f;

                            Color32 c = GetCameraPixelBilinear(cameraPixels, (int)CubemapFace.PositiveY, u, v);
                            rTotal += c.r; gTotal += c.g; bTotal += c.b; aTotal += c.a;
                        }

                    int baseIdx = stride * (panoramaHeight - 1 - y) + x * 4;
                    pixelValues[baseIdx + 0] = (byte)(bTotal / numPixelsAveraged);
                    pixelValues[baseIdx + 1] = (byte)(gTotal / numPixelsAveraged);
                    pixelValues[baseIdx + 2] = (byte)(rTotal / numPixelsAveraged);
                    pixelValues[baseIdx + 3] = (byte)(aTotal / numPixelsAveraged);

                    if ((x & 0xFF) == 0 && Time.realtimeSinceStartup - startTime > processingTimePerFrame)
                    {
                        yield return null; // Wait until next frame
                        startTime = Time.realtimeSinceStartup;
                    }
                }
        }

        private IEnumerator CubemapToEquirectangularCpuNegativeY(uint[] cameraPixels, byte[] pixelValues, int stride, int panoramaWidth, int panoramaHeight, int ssaaFactor, float startTime, float processingTimePerFrame, int numPixelsAveraged,
            int startX, int startY, int endX, int endY)
        {
            for (int y = startY; y < endY; y++)
                for (int x = startX; x < endX; x++)
                {
                    int rTotal = 0, gTotal = 0, bTotal = 0, aTotal = 0;
                    for (int ySsaa = y * ssaaFactor; ySsaa < (y + 1) * ssaaFactor; ySsaa++)
                        for (int xSsaa = x * ssaaFactor; xSsaa < (x + 1) * ssaaFactor; xSsaa++)
                        {
                            float xcoord = (float)xSsaa / (panoramaWidth * ssaaFactor);
                            float ycoord = (float)ySsaa / (panoramaHeight * ssaaFactor);

                            float latitude = (ycoord - 0.5f) * Mathf.PI;
                            float longitude = (xcoord * 2.0f - 1.0f) * Mathf.PI;

                            float cosLat = Mathf.Cos(latitude);
                            Vector3 equirectRayDirection = new Vector3(
                                cosLat * Mathf.Sin(longitude), -Mathf.Sin(latitude), cosLat * Mathf.Cos(longitude));

                            float distance = 1.0f / equirectRayDirection.y;
                            float u = equirectRayDirection.x * distance, v = equirectRayDirection.z * distance;
                            u = -u;

                            u = (u + 1.0f) / 2.0f;
                            v = (v + 1.0f) / 2.0f;

                            Color32 c = GetCameraPixelBilinear(cameraPixels, (int)CubemapFace.NegativeY, u, v);
                            rTotal += c.r; gTotal += c.g; bTotal += c.b; aTotal += c.a;
                        }

                    int baseIdx = stride * (panoramaHeight - 1 - y) + x * 4;
                    pixelValues[baseIdx + 0] = (byte)(bTotal / numPixelsAveraged);
                    pixelValues[baseIdx + 1] = (byte)(gTotal / numPixelsAveraged);
                    pixelValues[baseIdx + 2] = (byte)(rTotal / numPixelsAveraged);
                    pixelValues[baseIdx + 3] = (byte)(aTotal / numPixelsAveraged);

                    if ((x & 0xFF) == 0 && Time.realtimeSinceStartup - startTime > processingTimePerFrame)
                    {
                        yield return null; // Wait until next frame
                        startTime = Time.realtimeSinceStartup;
                    }
                }
        }

        private IEnumerator CubemapToEquirectangularCpuPositiveX(uint[] cameraPixels, byte[] pixelValues, int stride, int panoramaWidth, int panoramaHeight, int ssaaFactor, float startTime, float processingTimePerFrame, int numPixelsAveraged,
            int startX, int startY, int endX, int endY)
        {
            for (int y = startY; y < endY; y++)
                for (int x = startX; x < endX; x++)
                {
                    int rTotal = 0, gTotal = 0, bTotal = 0, aTotal = 0;
                    for (int ySsaa = y * ssaaFactor; ySsaa < (y + 1) * ssaaFactor; ySsaa++)
                        for (int xSsaa = x * ssaaFactor; xSsaa < (x + 1) * ssaaFactor; xSsaa++)
                        {
                            float xcoord = (float)xSsaa / (panoramaWidth * ssaaFactor);
                            float ycoord = (float)ySsaa / (panoramaHeight * ssaaFactor);

                            float latitude = (ycoord - 0.5f) * Mathf.PI;
                            float longitude = (xcoord * 2.0f - 1.0f) * Mathf.PI;

                            float cosLat = Mathf.Cos(latitude);
                            Vector3 equirectRayDirection = new Vector3(
                                cosLat * Mathf.Sin(longitude), -Mathf.Sin(latitude), cosLat * Mathf.Cos(longitude));

                            float distance = 1.0f / equirectRayDirection.x;
                            float u = -equirectRayDirection.z * distance, v = equirectRayDirection.y * distance;
                            v = -v;

                            u = (u + 1.0f) / 2.0f;
                            v = (v + 1.0f) / 2.0f;

                            Color32 c = GetCameraPixelBilinear(cameraPixels, (int)CubemapFace.PositiveX, u, v);
                            rTotal += c.r; gTotal += c.g; bTotal += c.b; aTotal += c.a;
                        }

                    int baseIdx = stride * (panoramaHeight - 1 - y) + x * 4;
                    pixelValues[baseIdx + 0] = (byte)(bTotal / numPixelsAveraged);
                    pixelValues[baseIdx + 1] = (byte)(gTotal / numPixelsAveraged);
                    pixelValues[baseIdx + 2] = (byte)(rTotal / numPixelsAveraged);
                    pixelValues[baseIdx + 3] = (byte)(aTotal / numPixelsAveraged);

                    if ((x & 0xFF) == 0 && Time.realtimeSinceStartup - startTime > processingTimePerFrame)
                    {
                        yield return null; // Wait until next frame
                        startTime = Time.realtimeSinceStartup;
                    }
                }
        }

        private IEnumerator CubemapToEquirectangularCpuNegativeX(uint[] cameraPixels, byte[] pixelValues, int stride, int panoramaWidth, int panoramaHeight, int ssaaFactor, float startTime, float processingTimePerFrame, int numPixelsAveraged,
            int startX, int startY, int endX, int endY)
        {
            for (int y = startY; y < endY; y++)
                for (int x = startX; x < endX; x++)
                {
                    int rTotal = 0, gTotal = 0, bTotal = 0, aTotal = 0;
                    for (int ySsaa = y * ssaaFactor; ySsaa < (y + 1) * ssaaFactor; ySsaa++)
                        for (int xSsaa = x * ssaaFactor; xSsaa < (x + 1) * ssaaFactor; xSsaa++)
                        {
                            float xcoord = (float)xSsaa / (panoramaWidth * ssaaFactor);
                            float ycoord = (float)ySsaa / (panoramaHeight * ssaaFactor);

                            float latitude = (ycoord - 0.5f) * Mathf.PI;
                            float longitude = (xcoord * 2.0f - 1.0f) * Mathf.PI;

                            float cosLat = Mathf.Cos(latitude);
                            Vector3 equirectRayDirection = new Vector3(
                                cosLat * Mathf.Sin(longitude), -Mathf.Sin(latitude), cosLat * Mathf.Cos(longitude));

                            float distance = 1.0f / equirectRayDirection.x;
                            float u = -equirectRayDirection.z * distance, v = equirectRayDirection.y * distance;

                            u = (u + 1.0f) / 2.0f;
                            v = (v + 1.0f) / 2.0f;

                            Color32 c = GetCameraPixelBilinear(cameraPixels, (int)CubemapFace.NegativeX, u, v);
                            rTotal += c.r; gTotal += c.g; bTotal += c.b; aTotal += c.a;
                        }

                    int baseIdx = stride * (panoramaHeight - 1 - y) + x * 4;
                    pixelValues[baseIdx + 0] = (byte)(bTotal / numPixelsAveraged);
                    pixelValues[baseIdx + 1] = (byte)(gTotal / numPixelsAveraged);
                    pixelValues[baseIdx + 2] = (byte)(rTotal / numPixelsAveraged);
                    pixelValues[baseIdx + 3] = (byte)(aTotal / numPixelsAveraged);

                    if ((x & 0xFF) == 0 && Time.realtimeSinceStartup - startTime > processingTimePerFrame)
                    {
                        yield return null; // Wait until next frame
                        startTime = Time.realtimeSinceStartup;
                    }
                }
        }

        private IEnumerator CubemapToEquirectangularCpuPositiveZ(uint[] cameraPixels, byte[] pixelValues, int stride, int panoramaWidth, int panoramaHeight, int ssaaFactor, float startTime, float processingTimePerFrame, int numPixelsAveraged,
            int startX, int startY, int endX, int endY)
        {
            for (int y = startY; y < endY; y++)
                for (int x = startX; x < endX; x++)
                {
                    int rTotal = 0, gTotal = 0, bTotal = 0, aTotal = 0;
                    for (int ySsaa = y * ssaaFactor; ySsaa < (y + 1) * ssaaFactor; ySsaa++)
                        for (int xSsaa = x * ssaaFactor; xSsaa < (x + 1) * ssaaFactor; xSsaa++)
                        {
                            float xcoord = (float)xSsaa / (panoramaWidth * ssaaFactor);
                            float ycoord = (float)ySsaa / (panoramaHeight * ssaaFactor);

                            float latitude = (ycoord - 0.5f) * Mathf.PI;
                            float longitude = (xcoord * 2.0f - 1.0f) * Mathf.PI;

                            float cosLat = Mathf.Cos(latitude);
                            Vector3 equirectRayDirection = new Vector3(
                                cosLat * Mathf.Sin(longitude), -Mathf.Sin(latitude), cosLat * Mathf.Cos(longitude));

                            float distance = 1.0f / equirectRayDirection.z;
                            float u = equirectRayDirection.x * distance, v = equirectRayDirection.y * distance;
                            v = -v;

                            u = (u + 1.0f) / 2.0f;
                            v = (v + 1.0f) / 2.0f;

                            Color32 c = GetCameraPixelBilinear(cameraPixels, (int)CubemapFace.PositiveZ, u, v);
                            rTotal += c.r; gTotal += c.g; bTotal += c.b; aTotal += c.a;
                        }

                    int baseIdx = stride * (panoramaHeight - 1 - y) + x * 4;
                    pixelValues[baseIdx + 0] = (byte)(bTotal / numPixelsAveraged);
                    pixelValues[baseIdx + 1] = (byte)(gTotal / numPixelsAveraged);
                    pixelValues[baseIdx + 2] = (byte)(rTotal / numPixelsAveraged);
                    pixelValues[baseIdx + 3] = (byte)(aTotal / numPixelsAveraged);

                    if ((x & 0xFF) == 0 && Time.realtimeSinceStartup - startTime > processingTimePerFrame)
                    {
                        yield return null; // Wait until next frame
                        startTime = Time.realtimeSinceStartup;
                    }
                }
        }

        private IEnumerator CubemapToEquirectangularCpuNegativeZ(uint[] cameraPixels, byte[] pixelValues, int stride, int panoramaWidth, int panoramaHeight, int ssaaFactor, float startTime, float processingTimePerFrame, int numPixelsAveraged,
            int startX, int startY, int endX, int endY)
        {
            for (int y = startY; y < endY; y++)
                for (int x = startX; x < endX; x++)
                {
                    int rTotal = 0, gTotal = 0, bTotal = 0, aTotal = 0;
                    for (int ySsaa = y * ssaaFactor; ySsaa < (y + 1) * ssaaFactor; ySsaa++)
                        for (int xSsaa = x * ssaaFactor; xSsaa < (x + 1) * ssaaFactor; xSsaa++)
                        {
                            float xcoord = (float)xSsaa / (panoramaWidth * ssaaFactor);
                            float ycoord = (float)ySsaa / (panoramaHeight * ssaaFactor);

                            float latitude = (ycoord - 0.5f) * Mathf.PI;
                            float longitude = (xcoord * 2.0f - 1.0f) * Mathf.PI;

                            float cosLat = Mathf.Cos(latitude);
                            Vector3 equirectRayDirection = new Vector3(
                                cosLat * Mathf.Sin(longitude), -Mathf.Sin(latitude), cosLat * Mathf.Cos(longitude));

                            float distance = 1.0f / equirectRayDirection.z;
                            float u = equirectRayDirection.x * distance, v = equirectRayDirection.y * distance;

                            u = (u + 1.0f) / 2.0f;
                            v = (v + 1.0f) / 2.0f;

                            Color32 c = GetCameraPixelBilinear(cameraPixels, (int)CubemapFace.NegativeZ, u, v);
                            rTotal += c.r; gTotal += c.g; bTotal += c.b; aTotal += c.a;
                        }

                    int baseIdx = stride * (panoramaHeight - 1 - y) + x * 4;
                    pixelValues[baseIdx + 0] = (byte)(bTotal / numPixelsAveraged);
                    pixelValues[baseIdx + 1] = (byte)(gTotal / numPixelsAveraged);
                    pixelValues[baseIdx + 2] = (byte)(rTotal / numPixelsAveraged);
                    pixelValues[baseIdx + 3] = (byte)(aTotal / numPixelsAveraged);

                    if ((x & 0xFF) == 0 && Time.realtimeSinceStartup - startTime > processingTimePerFrame)
                    {
                        yield return null; // Wait until next frame
                        startTime = Time.realtimeSinceStartup;
                    }
                }
        }

        private IEnumerator CubemapToEquirectangularCpuGeneralCase(uint[] cameraPixels, byte[] pixelValues, int stride, int panoramaWidth, int panoramaHeight, int ssaaFactor, float startTime, float processingTimePerFrame, float maxWidth, float maxHeight, int numPixelsAveraged,
            int startX, int startY, int endX, int endY)
        {
            for (int y = startY; y < endY; y++)
                for (int x = startX; x < endX; x++)
                {
                    int rTotal = 0, gTotal = 0, bTotal = 0, aTotal = 0;
                    for (int ySsaa = y * ssaaFactor; ySsaa < (y + 1) * ssaaFactor; ySsaa++)
                        for (int xSsaa = x * ssaaFactor; xSsaa < (x + 1) * ssaaFactor; xSsaa++)
                        {
                            float xcoord = (float)xSsaa / (panoramaWidth * ssaaFactor);
                            float ycoord = (float)ySsaa / (panoramaHeight * ssaaFactor);

                            float latitude = (ycoord - 0.5f) * Mathf.PI;
                            float longitude = (xcoord * 2.0f - 1.0f) * Mathf.PI;

                            // Equivalent to: Vector3 equirectRayDirection =
                            //     Quaternion.Euler(-latitude * 360/(2*Mathf.PI), longitude * 360/(2*Mathf.PI), 0.0f) * new Vector3(0, 0, 1);
                            float cosLat = Mathf.Cos(latitude);
                            Vector3 equirectRayDirection = new Vector3(
                                cosLat * Mathf.Sin(longitude), -Mathf.Sin(latitude), cosLat * Mathf.Cos(longitude));

                            float distance = 0.0f;
                            CubemapFace face;
                            float u, v;

                            {
                                distance = 1.0f / equirectRayDirection.y;
                                u = equirectRayDirection.x * distance; v = equirectRayDirection.z * distance;
                                if (equirectRayDirection.y > 0.0f)
                                {
                                    face = CubemapFace.PositiveY;
                                }
                                else
                                {
                                    face = CubemapFace.NegativeY;
                                    u = -u;
                                }
                            }

                            if (Mathf.Abs(u) > 1.0f || Mathf.Abs(v) > 1.0f)
                            {
                                distance = 1.0f / equirectRayDirection.x;
                                u = -equirectRayDirection.z * distance; v = equirectRayDirection.y * distance;
                                if (equirectRayDirection.x > 0.0f)
                                {
                                    face = CubemapFace.PositiveX;
                                    v = -v;
                                }
                                else
                                {
                                    face = CubemapFace.NegativeX;
                                }
                            }
                            if (Mathf.Abs(u) > 1.0f || Mathf.Abs(v) > 1.0f)
                            {
                                distance = 1.0f / equirectRayDirection.z;
                                u = equirectRayDirection.x * distance; v = equirectRayDirection.y * distance;
                                if (equirectRayDirection.z > 0.0f)
                                {
                                    face = CubemapFace.PositiveZ;
                                    v = -v;
                                }
                                else
                                {
                                    face = CubemapFace.NegativeZ;
                                }
                            }

                            u = (u + 1.0f) / 2.0f;
                            v = (v + 1.0f) / 2.0f;

                            // Boundary: should blend between cubemap views, but for now just grab color
                            // of nearest pixel in selected cubemap view
                            u = Mathf.Min(u, maxWidth);
                            v = Mathf.Min(v, maxHeight);

                            Color32 c = GetCameraPixelBilinear(cameraPixels, (int)face, u, v);
                            rTotal += c.r; gTotal += c.g; bTotal += c.b; aTotal += c.a;
                        }

                    int baseIdx = stride * (panoramaHeight - 1 - y) + x * 4;
                    pixelValues[baseIdx + 0] = (byte)(bTotal / numPixelsAveraged);
                    pixelValues[baseIdx + 1] = (byte)(gTotal / numPixelsAveraged);
                    pixelValues[baseIdx + 2] = (byte)(rTotal / numPixelsAveraged);
                    pixelValues[baseIdx + 3] = (byte)(aTotal / numPixelsAveraged);

                    if ((x & 0xFF) == 0 && Time.realtimeSinceStartup - startTime > processingTimePerFrame)
                    {
                        yield return null; // Wait until next frame
                        startTime = Time.realtimeSinceStartup;
                    }
                }
        }
    }
}
