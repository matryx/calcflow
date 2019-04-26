using UnityEngine;
using System.Collections;
using System;
using System.Collections.Generic;

public class OvrAvatarProjectorRenderComponent : OvrAvatarRenderComponent {

    Material material;

    internal void InitializeProjectorRender(ovrAvatarRenderPart_ProjectorRender render, Shader shader, OvrAvatarRenderComponent target)
    {
        if (shader == null)
        {
            shader = Shader.Find("OvrAvatar/AvatarSurfaceShader");
        }

        material = CreateAvatarMaterial(gameObject.name + "_projector", shader);
        material.EnableKeyword("PROJECTOR_ON");

        Renderer renderer = target.GetComponent<Renderer>();
        if (renderer != null)
        {
            List<Material> materials = new List<Material>(renderer.sharedMaterials);
            materials.Add(material);
            renderer.sharedMaterials = materials.ToArray();
        }
    }

    internal void UpdateProjectorRender(OvrAvatarComponent component, ovrAvatarRenderPart_ProjectorRender render)
    {
        OvrAvatar.ConvertTransform(render.localTransform, this.transform);
        material.SetMatrix("_ProjectorWorldToLocal", this.transform.worldToLocalMatrix);
        component.UpdateAvatarMaterial(material, render.materialState);
        
    }

    void OnDrawGizmos()
    {
        Vector3 v000 = transform.localToWorldMatrix.MultiplyPoint(new Vector3(-1.0f, -1.0f, -1.0f));
        Vector3 v100 = transform.localToWorldMatrix.MultiplyPoint(new Vector3(+1.0f, -1.0f, -1.0f));
        Vector3 v010 = transform.localToWorldMatrix.MultiplyPoint(new Vector3(-1.0f, +1.0f, -1.0f));
        Vector3 v110 = transform.localToWorldMatrix.MultiplyPoint(new Vector3(+1.0f, +1.0f, -1.0f));
        Vector3 v001 = transform.localToWorldMatrix.MultiplyPoint(new Vector3(-1.0f, -1.0f, +1.0f));
        Vector3 v101 = transform.localToWorldMatrix.MultiplyPoint(new Vector3(+1.0f, -1.0f, +1.0f));
        Vector3 v011 = transform.localToWorldMatrix.MultiplyPoint(new Vector3(-1.0f, +1.0f, +1.0f));
        Vector3 v111 = transform.localToWorldMatrix.MultiplyPoint(new Vector3(+1.0f, +1.0f, +1.0f));

        Gizmos.color = Color.gray;

        Gizmos.DrawLine(v000, v100);
        Gizmos.DrawLine(v000, v010);
        Gizmos.DrawLine(v010, v110);
        Gizmos.DrawLine(v100, v110);

        Gizmos.DrawLine(v000, v001);
        Gizmos.DrawLine(v100, v101);
        Gizmos.DrawLine(v010, v011);
        Gizmos.DrawLine(v110, v111);

        Gizmos.DrawLine(v001, v101);
        Gizmos.DrawLine(v001, v011);
        Gizmos.DrawLine(v011, v111);
        Gizmos.DrawLine(v101, v111);
    }
}
