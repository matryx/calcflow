using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Textures : MonoBehaviour {

    public static void SetTexture(string textureName, ref GameObject toSet)
    {
        toSet.SetActive(true);
        MeshRenderer meshRenderer = toSet.GetComponent<MeshRenderer>();
        Texture2D tex = Resources.Load<Texture2D>(textureName);
        Material rendererMaterial = new Material(meshRenderer.material);
        rendererMaterial.mainTexture = tex;
        meshRenderer.material = rendererMaterial;
    }
}