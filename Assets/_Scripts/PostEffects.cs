using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class PostEffects : MonoBehaviour {

    Camera _cam;
    public Shader renderBW;
    public Shader glow;
    Camera temp;
    Material postMat;

	// Use this for initialization
	void Start () {
        _cam = GetComponent<Camera>();
        temp = new GameObject().AddComponent<Camera>();
        DontDestroyOnLoad(temp);
        temp.enabled = false;
        postMat = new Material(glow);
	}

    void OnRenderImage(RenderTexture source, RenderTexture destination)
    {
        temp.CopyFrom(_cam);
        temp.clearFlags = CameraClearFlags.Color;
        temp.backgroundColor = Color.black;

        temp.cullingMask = 1 << LayerMask.NameToLayer("Glow");
        RenderTexture rt = new RenderTexture(source.width, source.height, 0, RenderTextureFormat.R8);
        rt.Create();
        temp.targetTexture = rt;

        temp.RenderWithShader(renderBW, "");

        postMat.SetTexture("_SceneTex", source);
        Graphics.Blit(rt, destination, postMat);
        rt.Release();
    }
}
