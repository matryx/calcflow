using UnityEngine;
using System.Collections;


namespace LSToolKit.Helpers
{
    public class LSFadeController : MonoBehaviour
    {

        public Material fadeSourceMaterial = null;

        int fadeValue = 0;

        void Start ()
        {
           // FadeIn();
        }

        void OnPostRender()
        {
            fadeSourceMaterial.SetPass(0);
            GL.PushMatrix();
            GL.LoadOrtho();
            GL.Color(fadeSourceMaterial.color);
            GL.Begin(GL.QUADS);
            GL.Vertex3(0f, 0f, -12f);
            GL.Vertex3(0f, 1f, -12f);
            GL.Vertex3(1f, 1f, -12f);
            GL.Vertex3(1f, 0f, -12f);
            GL.End();
            GL.PopMatrix();
        }

        /*
        public void FadeOut ()
        {
            iTween.ValueTo(gameObject, iTween.Hash(
                "from", 0,
                "to", 255,
                "time", 1.0f,
                "onupdatetarget", gameObject,
                "onupdate", "tweenOnUpdateCallBack",
                "easetype", iTween.EaseType.easeOutQuad
                )
            );
        }

        public void FadeIn()
        {
            iTween.ValueTo(gameObject, iTween.Hash(
                "from", 255,
                "to", 0,
                "time", 1.0f,
                "onupdatetarget", gameObject,
                "onupdate", "tweenOnUpdateCallBack",
                "easetype", iTween.EaseType.easeOutQuad
                )
            );
        }

        void tweenOnUpdateCallBack(int newValue)
        {
            float fade  = (float)newValue / 255.0f;
            Color color = new Color(fadeSourceMaterial.color.r, fadeSourceMaterial.color.g, fadeSourceMaterial.color.b);
            color.a = fade;
            fadeSourceMaterial.color = color;
        }
        */
    }
}

