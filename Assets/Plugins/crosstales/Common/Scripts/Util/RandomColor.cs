using UnityEngine;

namespace Crosstales.Common.Util
{
    /// <summary>Random color changer.</summary>
    //[HelpURL("https://www.crosstales.com/media/data/assets/radio/api/class_crosstales_1_1_radio_1_1_demo_1_1_util_1_1_random_color.html")] //TODO update URL
    public class RandomColor : MonoBehaviour
    {
        #region Variables

        ///<summary>Use intervals to change the color (default: true).</summary>
        [Tooltip("Use intervals to change the color (default: true).")]
        public bool UseInterval = true;

        ///<summary>Random change interval between min (= x) and max (= y) in seconds (default: x = 5, y = 10).</summary>
        [Tooltip("Random change interval between min (= x) and max (= y) in seconds (default: x = 5, y = 10).")]
        public Vector2 ChangeInterval = new Vector2(5, 10);


        ///<summary>Random hue range between min (= x) and max (= y) (default: x = 0, y = 1).</summary>
        [Tooltip("Random hue range between min (= x) and max (= y) (default: x = 0, y = 1).")]
        public Vector2 HueRange = new Vector2(0f, 1f);

        ///<summary>Random saturation range between min (= x) and max (= y) (default: x = 1, y = 1).</summary>
        [Tooltip("Random saturation range between min (= x) and max (= y) (default: x = 1, y = 1).")]
        public Vector2 SaturationRange = new Vector2(1f, 1f);

        ///<summary>Random value range between min (= x) and max (= y) (default: x = 1, y = 1).</summary>
        [Tooltip("Random value range between min (= x) and max (= y) (default: x = 1, y = 1).")]
        public Vector2 ValueRange = new Vector2(1f, 1f);

        ///<summary>Random alpha range between min (= x) and max (= y) (default: x = 1, y = 1).</summary>
        [Tooltip("Random alpha range between min (= x) and max (= y) (default: x = 1, y = 1).")]
        public Vector2 AlphaRange = new Vector2(1f, 1f);

        ///<summary>Use gray scale colors (default: false).</summary>
        [Tooltip("Use gray scale colors (default: false).")]
        public bool GrayScale = false;

        ///<summary>Modify the color of a material instead of the Renderer (default: not set, optional).</summary>
        [Tooltip("Modify the color of a material instead of the Renderer (default: not set, optional).")]
        public Material Material;

        ///<summary>Set the object to a random color at Start (default: false).</summary>
        [Tooltip("Set the object to a random color at Start (default: false).")]
        public bool RandomColorAtStart = false;

        private float elapsedTime = 0f;
        private float changeTime = 0f;
        private Renderer currentRenderer;

        private Color32 startColor;
        private Color32 endColor;

        private float lerpProgress = 0f;

        #endregion


        #region MonoBehaviour methods

        public void Start()
        {
            elapsedTime = changeTime = Random.Range(ChangeInterval.x, ChangeInterval.y);

            if (RandomColorAtStart)
            {
                if (GrayScale)
                {
                    float grayScale = Random.Range(HueRange.x, HueRange.y);
                    startColor = new Color(grayScale, grayScale, grayScale, Random.Range(AlphaRange.x, AlphaRange.y));
                }
                else
                {
                    startColor = Random.ColorHSV(HueRange.x, HueRange.y, SaturationRange.x, SaturationRange.y, ValueRange.x, ValueRange.y, AlphaRange.x, AlphaRange.y);
                }

                if (Material != null)
                {
                    Material.SetColor("_Color", startColor);
                }
                else
                {
                    currentRenderer = GetComponent<Renderer>();
                    currentRenderer.material.color = startColor;
                }
            }
            else
            {
                if (Material != null)
                {
                    startColor = Material.GetColor("_Color");
                }
                else
                {
                    currentRenderer = GetComponent<Renderer>();
                    startColor = currentRenderer.material.color;
                }
            }
        }

        public void Update()
        {
            if (UseInterval)
            {
                elapsedTime += Time.deltaTime;

                if (elapsedTime > changeTime)
                {
                    lerpProgress = elapsedTime = 0f;

                    if (GrayScale)
                    {
                        float grayScale = Random.Range(HueRange.x, HueRange.y);
                        endColor = new Color(grayScale, grayScale, grayScale, Random.Range(AlphaRange.x, AlphaRange.y));
                    }
                    else
                    {
                        endColor = Random.ColorHSV(HueRange.x, HueRange.y, SaturationRange.x, SaturationRange.y, ValueRange.x, ValueRange.y, AlphaRange.x, AlphaRange.y);
                    }

                    changeTime = Random.Range(ChangeInterval.x, ChangeInterval.y);
                }

                if (Material != null)
                {
                    Material.SetColor("_Color", Color.Lerp(startColor, endColor, lerpProgress));
                }
                else
                {
                    currentRenderer.material.color = Color.Lerp(startColor, endColor, lerpProgress);
                }

                if (lerpProgress < 1f)
                {
                    lerpProgress += Time.deltaTime / (changeTime - 0.1f);
                }
                else
                {
                    if (Material != null)
                    {
                        startColor = Material.GetColor("_Color");
                    }
                    else
                    {
                        startColor = currentRenderer.material.color;
                    }
                }
            }
        }

        #endregion
    }
}
// © 2015-2019 crosstales LLC (https://www.crosstales.com)