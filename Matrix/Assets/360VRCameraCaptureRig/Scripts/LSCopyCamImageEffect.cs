using UnityEngine;
using System;
using System.Collections.Generic;
using System.Reflection;

namespace LSToolKit.Helpers
{
    public class LSCopyCamImageEffect : MonoBehaviour
    {
        public struct InstanceMethodPair
        {
            public object Instance;
            public MethodInfo Method;
        }
        public List<InstanceMethodPair> onRenderImageMethods = new List<InstanceMethodPair>();

        public static List<InstanceMethodPair> GenerateMethodList(Camera camToCopy)
        {
            var result = new List<InstanceMethodPair>();
            foreach (var script in camToCopy.gameObject.GetComponents<MonoBehaviour>())
            {
                if (script.enabled)
                {
                    Type scriptType = script.GetType();
                    MethodInfo m = scriptType.GetMethod("OnRenderImage",
                        BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic, null,
                        new Type[] { typeof(RenderTexture), typeof(RenderTexture) }, null);
                    if (m != null)
                    {
                        InstanceMethodPair pair = new InstanceMethodPair();
                        pair.Instance = script;
                        pair.Method = m;
                        result.Add(pair);
                    }
                }
            }
            return result;
        }

        RenderTexture[] temp = new RenderTexture[] { null, null };

        void OnDestroy()
        {
            for (int i = 0; i < temp.Length; i++)
            {
                if (temp[i] != null)
                    Destroy(temp[i]);
                temp[i] = null;
            }
        }

        void OnRenderImage(RenderTexture src, RenderTexture dest)
        {
            int desiredDepth = Math.Max(src.depth, dest.depth);

            for (int i = 0; i < temp.Length; i++)
            {
                if (onRenderImageMethods.Count > i + 1)
                {
                    if (temp[i] != null &&
                        (temp[i].width != dest.width || temp[i].height != dest.height || temp[i].depth != desiredDepth || temp[i].format != dest.format))
                    {
                        Destroy(temp[i]);
                        temp[i] = null;
                    }
                    if (temp[i] == null)
                        temp[i] = new RenderTexture(dest.width, dest.height, desiredDepth, dest.format);
                }
            }

            var sequence = new List<RenderTexture>();
            sequence.Add(src);
            for (int i = 0; i < onRenderImageMethods.Count - 1; i++)
                sequence.Add(i % 2 == 0 ? temp[0] : temp[1]);
            sequence.Add(dest);

            for (int i = 0; i < onRenderImageMethods.Count; i++)
                onRenderImageMethods[i].Method.Invoke(onRenderImageMethods[i].Instance, new object[] { sequence[i], sequence[i + 1] });
        }
    }
}
