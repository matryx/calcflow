///////////////////////////////////////////////
// MKGlowSystem								 //
//											 //
// Created by Michael Kremmel on 23.12.2014  //
// Copyright © 2015 All rights reserved.     //
///////////////////////////////////////////////

using UnityEngine;
using System;
using System.Reflection;
using System.Collections;

namespace MKGlowSystem
{
    [ExecuteInEditMode]
    [RequireComponent(typeof(Camera))]
    public class MKGlow : MonoBehaviour
    {
        #region Get/Set
        private GameObject GlowCameraObject
        {
            get
            {
                if (!m_GlowCameraObject)
                {
                    m_GlowCameraObject = new GameObject("m_GlowCameraObject");
                    m_GlowCameraObject.hideFlags = HideFlags.HideAndDontSave;
                    m_GlowCameraObject.AddComponent<Camera>();
                    GlowCamera.orthographic = false;
                    GlowCamera.enabled = false;
                    GlowCamera.renderingPath = RenderingPath.VertexLit;
					GlowCamera.hideFlags = HideFlags.HideAndDontSave;
                }
                return m_GlowCameraObject;
            }
        }
        private Camera GlowCamera
        {
            get
            {
                if (m_GlowCamera == null)
                {
                    m_GlowCamera = GlowCameraObject.GetComponent<Camera>();
                }
                return m_GlowCamera;
            }
        }
        public GlowBlurCurve GlowCurve
        {
            get { return this.m_GlowCurve; }
            set { this.m_GlowCurve = value; }
        }
        public LayerMask GlowRenderLayer
        {
            get { return this.m_GlowRenderLayer; }
            set { this.m_GlowRenderLayer = value; }
        }
        public bool ShowCutoutGlow
        {
            get { return this.m_ShowCutoutGlow; }
            set { this.m_ShowCutoutGlow = value; }
        }
        public MKGlowMode GlowResolution
        {
            get { return this.m_GlowResolution; }
            set { this.m_GlowResolution = value; }
        }
        public bool ShowTransparentGlow
        {
            get { return this.m_ShowTransparentGlow; }
            set { this.m_ShowTransparentGlow = value; }
        }
        public MKGlowQuality GlowQuality
        {
            get { return this.m_GlowQuality; }
            set { this.m_GlowQuality = value; }
        }
        public MKGlowType GlowType
        {
            get { return this.m_GlowType; }
            set { this.m_GlowType = value; }
        }
        public Color FullScreenGlowTint
        {
            get { return this.m_FullScreenGlowTint; }
            set { this.m_FullScreenGlowTint = value; }
        }
        public int Samples
        {
            get { return this.m_Samples; }
            set { this.m_Samples = value; }
        }
        public int BlurIterations
        {
            get { return this.m_BlurIterations; }
            set { this.m_BlurIterations = value; }
        }
        public float BlurOffset
        {
            get { return this.m_BlurOffset; }
            set { this.m_BlurOffset = value; }
        }
        public float GlowIntensity
        {
            get { return this.m_GlowIntensity; }
            set { this.m_GlowIntensity = value; }
        }
        public float BlurSpread
        {
            get { return this.m_BlurSpread; }
            set { this.m_BlurSpread = value; }
        }
        internal Material FSDSMaterial
        {
            get
            {
                if (!m_FSDSMaterial)
                {
                    m_FSDSMaterial = new Material(m_FSDSShader);
                    m_FSDSMaterial.hideFlags = HideFlags.HideAndDontSave;
                }
                return m_FSDSMaterial;
            }
        }
        internal Material FastBlurMaterial
        {
            get
            {
                if (!m_FastBlurMaterial)
                {
                    m_FastBlurMaterial = new Material(m_NoGarbageBlurShader);
                    m_FastBlurMaterial.hideFlags = HideFlags.HideAndDontSave;
                }
                return m_FastBlurMaterial;
            }
        }
        internal Material BlurMaterial
        {
            get
            {
                if (m_BlurMaterial == null)
                {
                    m_BlurMaterial = new Material(m_BlurShader);
                    m_BlurMaterial.hideFlags = HideFlags.HideAndDontSave;
                }
                return m_BlurMaterial;
            }
        }

        internal Material CompositeMaterial
        {
            get
            {
                if (m_CompositeMaterial == null)
                {
                    m_CompositeMaterial = new Material(m_CompositeShader);
                    m_CompositeMaterial.hideFlags = HideFlags.HideAndDontSave;
                }
                return m_CompositeMaterial;
            }
        }
        #endregion


        #region Constants
        private static float[] gaussFilter = new float[11]
		{
			0.402f,0.623f,0.877f,1.120f,1.297f,1.362f,1.297f,1.120f,0.877f,0.623f,0.402f
		};
        #endregion


        #region shaders
        [SerializeField]
        private Shader m_NoGarbageBlurShader;
        [SerializeField]
        private Shader m_BlurShader;
        [SerializeField]
        private Shader m_CompositeShader;
        [SerializeField]
        private Shader m_GlowRenderShader;
        [SerializeField]
        private Shader m_FSDSShader;
        #endregion


        #region privates
        private Material m_CompositeMaterial;
        private Material m_FastBlurMaterial;
        private Material m_BlurMaterial;
        private Material m_FSDSMaterial;

        private Camera m_GlowCamera;
        private GameObject m_GlowCameraObject;
        private RenderTexture m_GlowTexture;

        [SerializeField]
        [Tooltip("The Glows blur calculation")]
        private GlowBlurCurve m_GlowCurve = GlowBlurCurve.Gauss;
        [SerializeField]
        [Tooltip("Renderlayer that should glow (only selective glow)")]
        private LayerMask m_GlowRenderLayer = -1;
        [SerializeField]
        [Tooltip("The resolution of the rendered glow")]
        private MKGlowMode m_GlowResolution = MKGlowMode.High;
        [SerializeField]
        [Tooltip("Show glow through Cutout rendered objects")]
        private bool m_ShowCutoutGlow = false;
        [SerializeField]
        [Tooltip("Show glow through Transparent rendered objects")]
        private bool m_ShowTransparentGlow = true;
        [SerializeField]
        [Tooltip("Selective = to specifically bring objects to glow, Fullscreen = complete screen glows")]
        private MKGlowType m_GlowType = MKGlowType.Selective;
        [SerializeField]
        [Tooltip("The main difference between Low and High is that Low has less Garbage Collection")]
        private MKGlowQuality m_GlowQuality = MKGlowQuality.High;
        [SerializeField]
        [Tooltip("The glows coloration in full screen mode (only FullscreenGlowType)")]
        private Color m_FullScreenGlowTint = new Color(1, 1, 1, 0);
        [SerializeField]
        [Tooltip("Width of the glow effect")]
        private float m_BlurSpread = 0.35f;
        [SerializeField]
        [Tooltip("Number of used blurs")]
        private int m_BlurIterations = 7;
        [SerializeField]
        [Tooltip("The global luminous intensity")]
        private float m_GlowIntensity = 0.3f;
        [SerializeField]
        [Tooltip("Distance to the object per blur")]
        private float m_BlurOffset = 0.0f;
        [SerializeField]
        [Tooltip("Significantly influences the blurs quality (recommended: 4)")]
        private int m_Samples = 2;
        #endregion

        private void Main()
        {
            if (m_GlowRenderShader == null)
            {
                enabled = false;
                Debug.LogWarning("Failed to load MKGlow Render Shader");
                return;
            }
            if (m_CompositeShader == null)
            {
                enabled = false;
                Debug.LogWarning("Failed to load MKGlow Composite Shader");
                return;
            }

            if (m_BlurShader == null)
            {
                enabled = false;
                Debug.LogWarning("Failed to load MKGlow Blur Shader");
                return;
            }

            if (m_NoGarbageBlurShader == null)
            {
                enabled = false;
                Debug.LogWarning("Failed to load MKGlow Fast Blur Shader");
                return;
            }
        }

        private void OnEnable()
        {
            SetupShaders();
        }

        private void Reset()
        {
            SetupShaders();
        }

        private void SetupKeywords()
        {
            if (ShowTransparentGlow)
            {
                Shader.EnableKeyword("MKTRANSPARENT_ON");
                Shader.DisableKeyword("MKTRANSPARENT_OFF");
            }
            else
            {
                Shader.DisableKeyword("MKTRANSPARENT_ON");
                Shader.EnableKeyword("MKTRANSPARENT_OFF");
            }
            if (ShowCutoutGlow)
            {
                Shader.EnableKeyword("MKCUTOUT_ON");
                Shader.DisableKeyword("MKCUTOUT_OFF");
            }
            else
            {
                Shader.DisableKeyword("MKCUTOUT_ON");
                Shader.EnableKeyword("MKCUTOUT_OFF");
            }
        }

        private void SetupShaders()
        {
            if (!m_BlurShader)
                m_BlurShader = Shader.Find("Hidden/MKGlowBlur");

            if (!m_NoGarbageBlurShader)
                m_NoGarbageBlurShader = Shader.Find("Hidden/MKGlowFastBlur");

            if (!m_CompositeShader)
                m_CompositeShader = Shader.Find("Hidden/MKGlowCompose");

            if (!m_GlowRenderShader)
                m_GlowRenderShader = Shader.Find("Hidden/MKGlowRender");

            if (!m_FSDSShader)
                m_FSDSShader = Shader.Find("Hidden/MKGlowFullScreenDownSample");
        }

        private void OnDisable()
        {
            if (m_CompositeMaterial)
            {
                DestroyImmediate(m_CompositeMaterial);
            }
            if (m_BlurMaterial)
            {
                DestroyImmediate(m_BlurMaterial);
            }
            if (m_FastBlurMaterial)
            {
                DestroyImmediate(m_FastBlurMaterial);
            }
            if (m_FSDSMaterial)
            {
                DestroyImmediate(m_FSDSMaterial);
            }

            if (m_GlowCamera)
                DestroyImmediate(GlowCamera);

            if (m_GlowCameraObject)
                DestroyImmediate(GlowCameraObject);

            if (m_GlowTexture)
            {
                RenderTexture.ReleaseTemporary(m_GlowTexture);
                DestroyImmediate(m_GlowTexture);
            }
        }

        private void SetupGlowCamera()
        {
            GlowCamera.CopyFrom(this.GetComponent<Camera>());
            GlowCamera.clearFlags = CameraClearFlags.SolidColor;
            GlowCamera.rect = new Rect(0, 0, 1, 1);
            GlowCamera.backgroundColor = new Color(0, 0, 0, 0);
            GlowCamera.cullingMask = GlowRenderLayer;
            GlowCamera.targetTexture = this.m_GlowTexture;
        }

        private void OnPreRender()
        {
            if (!gameObject.activeSelf || !this.enabled)
                return;

            if (m_GlowTexture != null)
            {
                RenderTexture.ReleaseTemporary(m_GlowTexture);
                m_GlowTexture = null;
            }

            if (GlowType == MKGlowType.Selective)
            {
                m_GlowTexture = RenderTexture.GetTemporary((int)((this.GetComponent<Camera>().pixelWidth) / CalculateSamples(ref m_GlowResolution)), (int)((this.GetComponent<Camera>().pixelHeight) / CalculateSamples(ref m_GlowResolution)), 16);
                SetupGlowCamera();
                SetupKeywords();
				if(GlowCamera.actualRenderingPath != RenderingPath.VertexLit)
					GlowCamera.renderingPath = RenderingPath.VertexLit;
                GlowCamera.RenderWithShader(this.m_GlowRenderShader, "RenderType");
            }
            else
            {
                if (GlowCamera)
                    DestroyImmediate(GlowCamera);
                if (GlowCameraObject)
                    DestroyImmediate(GlowCameraObject);
            }

            Mathf.Clamp(BlurSpread, 0.2f, 2f);
            Mathf.Clamp(BlurIterations, 0, 11);
            Mathf.Clamp(BlurOffset, 0.0f, 4.0f);
            Mathf.Clamp(Samples, 2, 16);
            Mathf.Clamp(GlowIntensity, 0f, 1f);
        }

        protected virtual void OnRenderImage(RenderTexture src, RenderTexture dest)
        {
            if (!gameObject.activeSelf || !this.enabled)
                return;

            if (GlowType == MKGlowType.Selective)
            {
                PerformSelectiveGlow(ref src, ref dest);
            }
            else
            {
                PerformFullScreenGlow(ref src, ref dest);
            }
        }

        private void PerformBlur(ref RenderTexture src, ref RenderTexture dest)
        {
            if (GlowQuality == MKGlowQuality.Low)
            {
                float off = (BlurOffset + BlurSpread);
                FastBlurMaterial.SetTexture("_MainTex", src);
                FastBlurMaterial.SetFloat("_Shift", off);
                Graphics.Blit(src, dest, FastBlurMaterial);
            }
            else
            {
                int offset = 1;
                Graphics.BlitMultiTap(src, dest, BlurMaterial,
                                      new Vector2(offset, offset),
                                      new Vector2(-offset, offset),
                                      new Vector2(offset, -offset),
                                      new Vector2(-offset, -offset)
                                      );
            }
        }

        private void PerformBlur(ref RenderTexture src, ref RenderTexture dest, int iteration)
        {
            float offset = BlurOffset + iteration * BlurSpread;
            if (GlowCurve == GlowBlurCurve.Gauss)
                offset *= gaussFilter[iteration];

            if (GlowQuality == MKGlowQuality.Low)
            {
                FastBlurMaterial.SetTexture("_MainTex", src);
                FastBlurMaterial.SetFloat("_Shift", offset);
                Graphics.Blit(src, dest, FastBlurMaterial);
            }
            else
            {
                Graphics.BlitMultiTap(src, dest, BlurMaterial,
                                      new Vector2(offset, offset),
                                      new Vector2(-offset, offset),
                                      new Vector2(offset, -offset),
                                      new Vector2(-offset, -offset)
                                      );
            }
        }

        private void PerformGlow(ref RenderTexture glowBuffer, ref RenderTexture dest, ref RenderTexture src)
        {
            CompositeMaterial.SetTexture("_GlowTex", src);
            Graphics.Blit(glowBuffer, dest, CompositeMaterial);
        }

        private void DSFS(RenderTexture source, RenderTexture dest)
        {
            FSDSMaterial.color = new Color(FullScreenGlowTint.r, FullScreenGlowTint.g, FullScreenGlowTint.b, FullScreenGlowTint.a);
            Graphics.Blit(source, dest, FSDSMaterial);
        }

        protected void PerformSelectiveGlow(ref RenderTexture source, ref RenderTexture dest)
        {
            Vector2 TextureSize;
            TextureSize.x = source.width / Samples;
            TextureSize.y = source.height / Samples;

            RenderTexture glowBuffer = RenderTexture.GetTemporary((int)TextureSize.x, (int)TextureSize.y, 0);

            if (GlowQuality == MKGlowQuality.Low)
                FastBlurMaterial.color = new Color(1F, 1F, 1F, GlowIntensity);
            else
                BlurMaterial.color = new Color(1F, 1F, 1F, GlowIntensity);

            PerformBlur(ref m_GlowTexture, ref glowBuffer);

            for (int i = 0; i < BlurIterations; i++)
            {
                RenderTexture glowBufferSecond = RenderTexture.GetTemporary((int)TextureSize.x, (int)TextureSize.y, 0);
                PerformBlur(ref glowBuffer, ref glowBufferSecond, i);
                RenderTexture.ReleaseTemporary(glowBuffer);
                glowBuffer = glowBufferSecond;
            }
            PerformGlow(ref glowBuffer, ref dest, ref source);

            RenderTexture.ReleaseTemporary(glowBuffer);

            if (this.m_GlowTexture != null)
            {
                RenderTexture.ReleaseTemporary(m_GlowTexture);
                this.m_GlowTexture = null;
            }
        }

        protected void PerformFullScreenGlow(ref RenderTexture source, ref RenderTexture destination)
        {
            Vector2 TextureSize;
            TextureSize.x = source.width / Samples;
            TextureSize.y = source.height / Samples;
            RenderTexture glowBuffer = RenderTexture.GetTemporary((int)TextureSize.x, (int)TextureSize.y, 0);

            if (GlowQuality == MKGlowQuality.Low)
                FastBlurMaterial.color = new Color(1F, 1F, 1F, GlowIntensity);
            else
                BlurMaterial.color = new Color(1F, 1F, 1F, GlowIntensity);

            DSFS(source, glowBuffer);

            for (int i = 0; i < BlurIterations; i++)
            {
                RenderTexture glowBufferSecond = RenderTexture.GetTemporary((int)TextureSize.x, (int)TextureSize.y, 0);
                PerformBlur(ref glowBuffer, ref glowBufferSecond, i);
                RenderTexture.ReleaseTemporary(glowBuffer);
                glowBuffer = glowBufferSecond;
            }
            Graphics.Blit(source, destination);

            CompositeMaterial.color = new Color(1F, 1F, 1F, Mathf.Clamp01(GlowIntensity));
            PerformGlow(ref glowBuffer, ref destination, ref source);

            RenderTexture.ReleaseTemporary(glowBuffer);
        }

        private int CalculateSamples(ref MKGlowMode resolution)
        {
            switch (GlowResolution)
            {
                case MKGlowMode.High:
                    {
                        return 1;
                    }
                case MKGlowMode.Normal:
                    {
                        return 2;
                    }
                case MKGlowMode.Mobile:
                    {
                        return 4;
                    }
                default:
                    {
                        return 1;
                    }
            }
        }
    }

    public enum MKGlowQuality
    {
        Low,
        High
    }

    public enum MKGlowType
    {
        Selective,
        Fullscreen
    }
    public enum MKGlowMode
    {
        High,
        Normal,
        Mobile
    }
    public enum GlowBlurCurve
    {
        Default,
        Gauss
    }
}