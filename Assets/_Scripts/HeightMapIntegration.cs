using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using AK;
using System;

public class HeightMapIntegration : MonoBehaviour
{

    public string u_min, u_max, v_min, v_max;
    private float uMin = -5, uMax = 5, vMin = -5, vMax = 5;
    public string expressionZ;

    ExpressionSolver solver;
    AK.Expression expZ;
    Variable varU, varV;

    List<Vector3> positions;
    List<Vector3> normals;
    List<Vector2> uvs;
    List<int> faces;

    [SerializeField] MeshFilter mesh;
    [SerializeField] MeshFilter left;
    [SerializeField] MeshFilter right;
    [SerializeField] MeshFilter back;
    [SerializeField] MeshFilter front;
    [SerializeField] MeshFilter bottom;

    [SerializeField] Transform sphere;

    // Use this for initialization
    void Start()
    {
        solver = new ExpressionSolver();
        expZ = new AK.Expression();
        //Variable varU = new Variable(), varV = new Variable();
        positions = new List<Vector3>();
        normals = new List<Vector3>();
        uvs = new List<Vector2>();
        faces = new List<int>();

        /* TEST */
        solver.SetGlobalVariable("x", -5);
        solver.SetGlobalVariable("y", -5);
        expZ = solver.SymbolicateExpression(expressionZ);
        varU = solver.GetGlobalVariable("x");
        varV = solver.GetGlobalVariable("y");
        uMin = (float)solver.EvaluateExpression(u_min); uMax = (float)solver.EvaluateExpression(u_max);
        vMin = (float)solver.EvaluateExpression(v_min); vMax = (float)solver.EvaluateExpression(v_max);
        TessellateSurface();
        left.mesh = TessellateCurve(0, 1, 0, null);
        right.mesh = TessellateCurve(0, 1, 1, null);
        back.mesh = TessellateCurve(0, null, 0, 1);
        front.mesh = TessellateCurve(1, null, 0, 1);
    }

    public void UpdateEquation()
    {
        Debug.Log("UpdateEquation()");
        positions = new List<Vector3>();
        normals = new List<Vector3>();
        uvs = new List<Vector2>();
        faces = new List<int>();

        try
        {
            expZ = solver.SymbolicateExpression(expressionZ);
            uMin = (float)solver.EvaluateExpression(u_min);
            uMax = (float)solver.EvaluateExpression(u_max);
            vMin = (float)solver.EvaluateExpression(v_min);
            vMax = (float)solver.EvaluateExpression(v_max);
        }
        catch (AK.ESSyntaxErrorException e)
        {
            return;
        }
        TessellateSurface();
        //StartCoroutine(TessellateSurface());
        sphere.GetComponent<ConstraintGrabbable>().SetXRange(uMin, uMax);
        sphere.GetComponent<ConstraintGrabbable>().SetZRange(vMin, vMax);
    }

    // Update is called once per frame
    void LateUpdate()
    {
        //AK.Expression expZ_ = null;
        //float uMin_ = -Mathf.Infinity, uMax_ = Mathf.Infinity, vMin_ = -Mathf.Infinity, vMax_ = Mathf.Infinity;
        //bool update = true;
        //try
        //{
        //    expZ_ = solver.SymbolicateExpression(expressionZ);
        //}
        //catch (Exception e)
        //{
        //    update = false;
        //}
        //try
        //{
        //    uMin_ = (float)solver.EvaluateExpression(u_min);
        //}
        //catch (Exception e)
        //{
        //    update = false;
        //}
        //try
        //{
        //    uMax_ = (float)solver.EvaluateExpression(u_max);
        //}
        //catch (Exception e)
        //{
        //    update = false;
        //}
        //try
        //{
        //    vMin_ = (float)solver.EvaluateExpression(v_min);
        //}
        //catch (Exception e)
        //{
        //    update = false;
        //}
        //try
        //{
        //    vMax_ = (float)solver.EvaluateExpression(v_max);
        //}
        //catch (Exception e)
        //{
        //    update = false;
        //}

        //if (update && !(expZ_ == null && expZ == expZ_ && uMin == uMin_ && uMax == uMax_ && vMin == vMin_ && vMax == vMax_))
        //{
        //    UpdateEquation();
        //}

        float u = sphere.GetComponent<ConstraintGrabbable>().lastLocalPos.x;
        float v = sphere.GetComponent<ConstraintGrabbable>().lastLocalPos.z;
        float uRange = uMax - uMin;
        float vRange = vMax - vMin;
        left.mesh = TessellateCurve(0, (u - uMin) / uRange, 0, null);
        right.mesh = TessellateCurve(0, (u - uMin) / uRange, (v - vMin) / vRange, null);
        back.mesh = TessellateCurve(0, null, 0, (v - vMin) / vRange);
        front.mesh = TessellateCurve((u - uMin) / uRange, null, 0, (v - vMin) / vRange);
        bottom.mesh = GetBottom(u, v);
    }

    Vector3 MapPoint01(float u, float v)
    {
        Vector2 uv = new Vector2(u, v);
        if (mappingCache.ContainsKey(uv))
        {
            return mappingCache[uv];
        }

        varU.value = uMin + (uMax - uMin) * u;
        varV.value = vMin + (vMax - vMin) * v;
        float z = (float)expZ.Evaluate();
        if (float.IsNaN(z)) z = 0f;
        if (float.IsNegativeInfinity(z)) z = float.MinValue;
        if (float.IsPositiveInfinity(z)) z = float.MaxValue;
        Vector3 result = new Vector3(uMin + (uMax - uMin) * u, z, vMin + (vMax - vMin) * v);
        ClampToBound(ref result);
        //mappingCache[uv] = new Vector3(uMin + (uMax - uMin) * u, z, vMin + (vMax - vMin) * v);
        mappingCache[uv] = result;
        return mappingCache[uv];
    }

    Vector3 GetNormal01(float u, float v)
    {
        Vector3 tanU = MapPoint01(u + 0.01f, v) - MapPoint01(u, v);
        Vector3 tanV = MapPoint01(u, v + 0.01f) - MapPoint01(u, v);
        return Vector3.Cross(tanU, tanV);
    }

    const float MaxRange = 20f;
    bool OnBoundingBox(Vector3 v)
    {
        return Mathf.Abs(v.x) >= MaxRange || Mathf.Abs(v.y) >= MaxRange || Mathf.Abs(v.z) >= MaxRange;
    }

    void ClampToBound(ref Vector3 v)
    {
        if (v.x >= MaxRange) v.x = MaxRange;
        if (v.y >= MaxRange) v.y = MaxRange;
        if (v.z >= MaxRange) v.z = MaxRange;
        if (v.x <= -MaxRange) v.x = -MaxRange;
        if (v.y <= -MaxRange) v.y = -MaxRange;
        if (v.z <= -MaxRange) v.z = -MaxRange;
    }

    Dictionary<Vector2, Vector3> mappingCache = new Dictionary<Vector2, Vector3>();
    float MinDist = 0.05f;
    public IEnumerator TessellateSurface()
    {

        float alloweddist = 0.1f;
        int i0, i1, i2, i3;
        float umin, umax, vmin, vmax;
        var positionIndex = new Dictionary<Vector3, int>();

        mappingCache = new Dictionary<Vector2, Vector3>();
        positions.Add(MapPoint01(0, 0)); normals.Add(GetNormal01(0, 0)); uvs.Add(new Vector2(0, 0));
        positions.Add(MapPoint01(0, 1)); normals.Add(GetNormal01(0, 1)); uvs.Add(new Vector2(0, 1));
        positions.Add(MapPoint01(1, 1)); normals.Add(GetNormal01(1, 1)); uvs.Add(new Vector2(1, 1));
        positions.Add(MapPoint01(1, 0)); normals.Add(GetNormal01(1, 0)); uvs.Add(new Vector2(1, 0));

        var indices = new Stack<int>();
        var uvRanges = new Stack<float>();

        positions.Add(MapPoint01(0.5f, 0.5f)); normals.Add(GetNormal01(0.5f, 0.5f)); uvs.Add(new Vector2(0.5f, 0.5f));
        int center = positions.Count - 1;
        positions.Add(MapPoint01(0, 0.5f)); normals.Add(GetNormal01(0, 0.5f)); uvs.Add(new Vector2(0, 0.5f));
        int xy = positions.Count - 1;
        positions.Add(MapPoint01(0.5f, 1)); normals.Add(GetNormal01(0.5f, 1)); uvs.Add(new Vector2(0.5f, 1));
        int yz = positions.Count - 1;
        positions.Add(MapPoint01(1, 0.5f)); normals.Add(GetNormal01(1, 0.5f)); uvs.Add(new Vector2(1, 0.5f));
        int zw = positions.Count - 1;
        positions.Add(MapPoint01(0.5f, 0)); normals.Add(GetNormal01(0.5f, 0)); uvs.Add(new Vector2(0.5f, 0));
        int wx = positions.Count - 1;

        indices.Push(0); indices.Push(xy); indices.Push(center); indices.Push(wx);
        uvRanges.Push(0); uvRanges.Push(0.5f); uvRanges.Push(0); uvRanges.Push(0.5f);
        indices.Push(xy); indices.Push(1); indices.Push(yz); indices.Push(center);
        uvRanges.Push(0); uvRanges.Push(0.5f); uvRanges.Push(0.5f); uvRanges.Push(1);
        indices.Push(center); indices.Push(yz); indices.Push(2); indices.Push(zw);
        uvRanges.Push(0.5f); uvRanges.Push(1); uvRanges.Push(0.5f); uvRanges.Push(1);
        indices.Push(wx); indices.Push(center); indices.Push(zw); indices.Push(3);
        uvRanges.Push(0.5f); uvRanges.Push(1); uvRanges.Push(0); uvRanges.Push(0.5f);

        while (indices.Count != 0)
        {
            i3 = indices.Pop(); i2 = indices.Pop(); i1 = indices.Pop(); i0 = indices.Pop();
            vmax = uvRanges.Pop(); vmin = uvRanges.Pop(); umax = uvRanges.Pop(); umin = uvRanges.Pop();

            if (positions.Count > 32000)
            {
                faces.Add(i0);
                faces.Add(i1);
                faces.Add(i2);
                faces.Add(i0);
                faces.Add(i2);
                faces.Add(i3);
                continue;
            }

            float umid = (umin + umax) / 2.0f;
            float vmid = (vmin + vmax) / 2.0f;

            // Vertex 1 <umin, vmin>
            Vector3 v0 = positions[i0];
            // Vertex 2 <umin, vmax>
            Vector3 v1 = positions[i1];
            // Vertex 3 <umax, vmax>
            Vector3 v2 = positions[i2];
            // Vertex 4 <umax, vmin>
            Vector3 v3 = positions[i3];

            //float maxDist = 40f;
            if (OnBoundingBox(v0) && OnBoundingBox(v1)&&OnBoundingBox(v2)&&OnBoundingBox(v3))
            {
                //positions[i0] = Clamp(v0); positions[i1] = Clamp(v1); positions[i2] = Clamp(v2); positions[i3] = Clamp(v3);
                //faces.Add(i0);
                //faces.Add(i1);
                //faces.Add(i2);
                //faces.Add(i0);
                //faces.Add(i2);
                //faces.Add(i3);
                continue;
            }

            if(Vector3.Distance(v0,v1)<MinDist
                || Vector3.Distance(v0, v2) < MinDist
                || Vector3.Distance(v0, v3) < MinDist
                || Vector3.Distance(v1, v2) < MinDist
                || Vector3.Distance(v1, v3) < MinDist
                || Vector3.Distance(v2, v3) < MinDist)
            {
                continue;
            }
            //ClampToBound(ref v0); ClampToBound(ref v1); ClampToBound(ref v2); ClampToBound(ref v3);

            bool cut_mid = false, cut_u_min = false, cut_u_max = false, cut_v_min = false, cut_v_max = false;

            // mid point criteria
            Vector3 mid = (v0 + v1 + v2 + v3) / 4.0f;
            Vector3 Mid = MapPoint01(umid, vmid);

            Vector3 umid_vmin = (v0 + v3) / 2.0f;
            Vector3 Umid_Vmin = MapPoint01(umid, vmin);

            Vector3 umid_vmax = (v1 + v2) / 2.0f;
            Vector3 Umid_Vmax = MapPoint01(umid, vmax);

            Vector3 vmid_umin = (v0 + v1) / 2.0f;
            Vector3 Vmid_Umin = MapPoint01(umin, vmid);

            Vector3 vmid_umax = (v2 + v3) / 2.0f;
            Vector3 Vmid_Umax = MapPoint01(umax, vmid);

            if (Vector3.Distance(Mid, mid) > alloweddist)
            {
                cut_mid = true;
            }
            Vector3 vec025 = (umid_vmin + v0) / 2.0f;
            Vector3 Vec025 = MapPoint01((umin + umid) / 2.0f, vmin);
            Vector3 vec075 = (umid_vmin + v3) / 2.0f;
            Vector4 Vec075 = MapPoint01((umax + umid) / 2.0f, vmin);
            if (Vector3.Distance(umid_vmin, Umid_Vmin) > alloweddist
                || Vector3.Distance(vec025, Vec025) > alloweddist || Vector3.Distance(vec075, Vec075) > alloweddist
                )
            {
                cut_u_min = true;
            }
            vec025 = (umid_vmax + v1) / 2.0f;
            Vec025 = MapPoint01((umid + umin) / 2.0f, vmax);
            vec075 = (umid_vmax + v2) / 2.0f;
            Vec075 = MapPoint01((umid + umax) / 2.0f, vmax);
            if (Vector3.Distance(umid_vmax, Umid_Vmax) > alloweddist
                || Vector3.Distance(vec025, Vec025) > alloweddist || Vector3.Distance(vec075, Vec075) > alloweddist
                )
            {
                cut_u_max = true;
            }
            vec025 = (vmid_umin + v0) / 2.0f;
            Vec025 = MapPoint01(umin, (vmid + vmin) / 2.0f);
            vec075 = (vmid_umin + v1) / 2.0f;
            Vec075 = MapPoint01(umin, (vmid + vmax) / 2.0f);
            if (Vector3.Distance(vmid_umin, Vmid_Umin) > alloweddist
                || Vector3.Distance(vec025, Vec025) > alloweddist || Vector3.Distance(vec075, Vec075) > alloweddist
                )
            {
                cut_v_min = true;
            }
            vec025 = (vmid_umax + v3) / 2.0f;
            Vec025 = MapPoint01(umax, (vmid + vmin) / 2.0f);
            vec075 = (vmid_umax + v2) / 2.0f;
            Vec075 = MapPoint01(umax, (vmid + vmax) / 2.0f);
            if (Vector3.Distance(vmid_umax, Vmid_Umax) > alloweddist
                || Vector3.Distance(vec025, Vec025) > alloweddist || Vector3.Distance(vec075, Vec075) > alloweddist
                )
            {
                cut_v_max = true;
            }
            // cut both directions
            if ((cut_u_max || cut_u_min) && (cut_v_max || cut_v_min))
            {
                Mid = cut_mid ? Mid : mid;
                Umid_Vmin = cut_u_min ? Umid_Vmin : umid_vmin;
                Umid_Vmax = cut_u_max ? Umid_Vmax : umid_vmax;
                Vmid_Umin = cut_v_min ? Vmid_Umin : vmid_umin;
                Vmid_Umax = cut_v_max ? Vmid_Umax : vmid_umax;

                int i4;
                if (positionIndex.ContainsKey(Mid))
                {
                    i4 = positionIndex[Mid];
                }
                else
                {
                    positions.Add(Mid);
                    normals.Add(cut_mid ? GetNormal01(umid, vmid) : (normals[i0] + normals[i1] + normals[i2] + normals[i3]) / 4.0f);
                    uvs.Add(new Vector2(umid, vmid));
                    i4 = positions.Count - 1;
                    positionIndex[Mid] = i4;
                }

                int i5;
                if (positionIndex.ContainsKey(Umid_Vmin))
                {
                    i5 = positionIndex[Umid_Vmin];
                }
                else
                {
                    positions.Add(Umid_Vmin);
                    normals.Add(cut_u_min ? GetNormal01(umid, vmin) :
                        (normals[i0] + normals[i3]) / 2.0f);
                    uvs.Add(new Vector2(umid, vmin));
                    i5 = positions.Count - 1;
                    positionIndex[Umid_Vmin] = i5;
                }

                int i6;
                if (positionIndex.ContainsKey(Umid_Vmax))
                {
                    i6 = positionIndex[Umid_Vmax];
                }
                else
                {
                    positions.Add(Umid_Vmax);
                    normals.Add(cut_u_max ? GetNormal01(umid, vmax) :
                        (normals[i1] + normals[i2]) / 2.0f);
                    uvs.Add(new Vector2(umid, vmax));
                    i6 = positions.Count - 1;
                    positionIndex[Umid_Vmax] = i6;
                }

                int i7;
                if (positionIndex.ContainsKey(Vmid_Umin))
                {
                    i7 = positionIndex[Vmid_Umin];
                }
                else
                {
                    positions.Add(Vmid_Umin);
                    normals.Add(cut_v_min ? GetNormal01(umin, vmid) :
                        (normals[i0] + normals[i1]) / 2.0f);
                    uvs.Add(new Vector2(umin, vmid));
                    i7 = positions.Count - 1;
                    positionIndex[Vmid_Umin] = i7;
                }

                int i8;
                if (positionIndex.ContainsKey(Vmid_Umax))
                {
                    i8 = positionIndex[Vmid_Umax];
                }
                else
                {
                    positions.Add(Vmid_Umax);
                    normals.Add(cut_v_max ? GetNormal01(umax, vmid) :
                        (normals[i2] + normals[i3]) / 2.0f);
                    uvs.Add(new Vector2(umax, vmid));
                    i8 = positions.Count - 1;
                    positionIndex[Vmid_Umax] = i8;
                }

                //Tessellate(i0, i7, i4, i5, umin, umid, vmin, vmid);
                indices.Push(i0); indices.Push(i7); indices.Push(i4); indices.Push(i5);
                uvRanges.Push(umin); uvRanges.Push(umid); uvRanges.Push(vmin); uvRanges.Push(vmid);
                //Tessellate(i7, i1, i6, i4, umin, umid, vmid, vmax);
                indices.Push(i7); indices.Push(i1); indices.Push(i6); indices.Push(i4);
                uvRanges.Push(umin); uvRanges.Push(umid); uvRanges.Push(vmid); uvRanges.Push(vmax);
                //Tessellate(i5, i4, i8, i3, umid, umax, vmin, vmid);
                indices.Push(i5); indices.Push(i4); indices.Push(i8); indices.Push(i3);
                uvRanges.Push(umid); uvRanges.Push(umax); uvRanges.Push(vmin); uvRanges.Push(vmid);
                //Tessellate(i4, i6, i2, i8, umid, umax, vmid, vmax);
                indices.Push(i4); indices.Push(i6); indices.Push(i2); indices.Push(i8);
                uvRanges.Push(umid); uvRanges.Push(umax); uvRanges.Push(vmid); uvRanges.Push(vmax);
            }
            else if ((cut_u_min || cut_u_max) && !(cut_v_min || cut_v_max))
            {
                Umid_Vmin = cut_u_min ? Umid_Vmin : umid_vmin;
                Umid_Vmax = cut_u_max ? Umid_Vmax : umid_vmax;

                int i4;
                if (positionIndex.ContainsKey(Umid_Vmin))
                {
                    i4 = positionIndex[Umid_Vmin];
                }
                else
                {
                    positions.Add(Umid_Vmin);
                    normals.Add(cut_u_min ? GetNormal01(umid, vmin) :
                        (normals[i0] + normals[i3]) / 2.0f);
                    uvs.Add(new Vector2(umid, vmin));
                    i4 = positions.Count - 1;
                    positionIndex[Umid_Vmin] = i4;
                }

                int i5;
                if (positionIndex.ContainsKey(Umid_Vmax))
                {
                    i5 = positionIndex[Umid_Vmax];
                }
                else
                {
                    positions.Add(Umid_Vmax);
                    normals.Add(cut_u_max ? GetNormal01(umid, vmax) :
                        (normals[i1] + normals[i2]) / 2.0f);
                    uvs.Add(new Vector2(umid, vmax));
                    i5 = positions.Count - 1;
                    positionIndex[Umid_Vmax] = i5;
                }

                indices.Push(i0); indices.Push(i1); indices.Push(i5); indices.Push(i4);
                uvRanges.Push(umin); uvRanges.Push(umid); uvRanges.Push(vmin); uvRanges.Push(vmax);

                indices.Push(i4); indices.Push(i5); indices.Push(i2); indices.Push(i3);
                uvRanges.Push(umid); uvRanges.Push(umax); uvRanges.Push(vmin); uvRanges.Push(vmax);
            }
            else if (!(cut_u_min || cut_u_max) && (cut_v_min || cut_v_max))
            {
                // split in v direction
                Vmid_Umin = cut_v_min ? Vmid_Umin : vmid_umin;
                Vmid_Umax = cut_v_max ? Vmid_Umax : vmid_umax;

                int i4;
                if (positionIndex.ContainsKey(Vmid_Umin))
                {
                    i4 = positionIndex[Vmid_Umin];
                }
                else
                {
                    positions.Add(Vmid_Umin);
                    normals.Add(cut_v_min ? GetNormal01(umin, vmid) :
                        (normals[i0] + normals[i1]) / 2.0f);
                    uvs.Add(new Vector2(umin, vmid));
                    i4 = positions.Count - 1;
                    positionIndex[Vmid_Umin] = i4;
                }

                int i5;
                if (positionIndex.ContainsKey(Vmid_Umax))
                {
                    i5 = positionIndex[Vmid_Umax];
                }
                else
                {
                    positions.Add(Vmid_Umax);
                    normals.Add(cut_v_max ? GetNormal01(umax, vmid) :
                        (normals[i2] + normals[i3]) / 2.0f);
                    uvs.Add(new Vector2(umax, vmid));
                    i5 = positions.Count - 1;
                    positionIndex[Vmid_Umax] = i5;
                }

                indices.Push(i0); indices.Push(i4); indices.Push(i5); indices.Push(i3);
                uvRanges.Push(umin); uvRanges.Push(umax); uvRanges.Push(vmin); uvRanges.Push(vmid);

                indices.Push(i4); indices.Push(i1); indices.Push(i2); indices.Push(i5);
                uvRanges.Push(umin); uvRanges.Push(umax); uvRanges.Push(vmid); uvRanges.Push(vmax);
            }
            else
            {
                //if (!OnBoundingBox(positions[i0]))
                    faces.Add(i0);
                //if (!OnBoundingBox(positions[i1]))
                    faces.Add(i1);
                //if (!OnBoundingBox(positions[i2]))
                    faces.Add(i2);
                //if (!OnBoundingBox(positions[i0]))
                    faces.Add(i0);
                //if (!OnBoundingBox(positions[i2]))
                    faces.Add(i2);
                //if (!OnBoundingBox(positions[i3]))
                    faces.Add(i3);
            }
            //mesh.mesh.Clear();
            //mesh.mesh.SetVertices(positions);
            //mesh.mesh.SetNormals(normals);
            //mesh.mesh.SetUVs(0, uvs);
            //mesh.mesh.SetTriangles(faces, 0);
            //yield return null;
        }

        //Debug.Log("Tessellation finished");

        //duplicate triangles for back face
        if (positions.Count < 32500)
        {
            int vertCount = positions.Count;
            for (int i = 0; i < vertCount; i++)
            {
                positions.Add(positions[i]);
                normals.Add(-normals[i]);
                uvs.Add(uvs[i]);
            }
            int triCount = faces.Count;
            for (int i = 0; i < triCount; i += 3)
            {
                faces.Add(faces[i + 2] + vertCount);
                faces.Add(faces[i + 1] + vertCount);
                faces.Add(faces[i] + vertCount);
            }
        }

        mesh.mesh.Clear();
        mesh.mesh.SetVertices(positions);
        mesh.mesh.SetNormals(normals);
        mesh.mesh.SetUVs(0, uvs);
        mesh.mesh.RecalculateNormals();
        mesh.mesh.SetTriangles(faces, 0);

        //Debug.Log("Mesh finalized");
        return null;
    }

    protected Mesh TessellateCurve(float umin, float? umax, float vmin, float? vmax)
    {
        if (vmax == null)
        {
            return TessellateUCurve(umin, umax.Value, vmin);
        }
        else
        {
            return TessellateVCurve(vmin, vmax.Value, umin);
        }
    }

    protected Mesh TessellateUCurve(float u_min, float u_max, float v)
    {
        float alloweddist = 0.05f;

        var positionIndex = new Dictionary<Vector3, int>();
        mappingCache = new Dictionary<Vector2, Vector3>();

        var sideVertices = new List<Vector3>();
        var sideFaces = new List<int>();
        var sideNormals = new List<Vector3>();

        var indices = new Stack<int>();
        var uMinMax = new Stack<float>();
        int i0, i1;
        float umin, umax;
        sideVertices.Add(MapPoint01(u_min, v)); sideVertices.Add(MapPoint01(u_max, v));
        sideNormals.Add(new Vector3(0, 0, 1)); sideNormals.Add(new Vector3(0, 0, 1));
        indices.Push(1); indices.Push(0);
        uMinMax.Push(u_max); uMinMax.Push(u_min);
        positionIndex[MapPoint01(u_min, v)] = 0;
        positionIndex[MapPoint01(u_max, v)] = 1;

        while (indices.Count != 0)
        {
            i0 = indices.Pop(); i1 = indices.Pop();
            umin = uMinMax.Pop(); umax = uMinMax.Pop();

            if (OnBoundingBox(sideVertices[i0]) && OnBoundingBox(sideVertices[i1]))
            {
                continue;
            }

            float umid = (umin + umax) / 2.0f;
            Vector3 mid = (sideVertices[i0] + sideVertices[i1]) / 2.0f;
            Vector3 Mid = MapPoint01(umid, v);
            Vector3 vec025 = (sideVertices[i0] + mid) / 2.0f;
            Vector3 Vec025 = MapPoint01((umid + umin) / 2.0f, v);
            Vector3 vec075 = (sideVertices[i1] + mid) / 2.0f;
            Vector3 Vec075 = MapPoint01((umid + umax) / 2.0f, v);

            if (Vector3.Distance(Mid, mid) > alloweddist || Vector3.Distance(Vec025, vec025) > alloweddist
                || Vector3.Distance(Vec075, vec075) > alloweddist)
            {
                int i2;
                sideVertices.Add(Mid);
                sideNormals.Add(new Vector3(0, 0, 1));
                i2 = sideVertices.Count - 1;
                positionIndex[Mid] = i2;

                indices.Push(i2); indices.Push(i0);
                uMinMax.Push(umid); uMinMax.Push(umin);
                indices.Push(i1); indices.Push(i2);
                uMinMax.Push(umax); uMinMax.Push(umid);
            }
            else
            {
                Vector3 min = sideVertices[i0];
                min.y = 0;
                int i2;
                if (positionIndex.ContainsKey(min))
                {
                    i2 = positionIndex[min];
                }
                else
                {
                    sideVertices.Add(min);
                    sideNormals.Add(new Vector3(0, 0, 1));
                    i2 = sideVertices.Count - 1;
                    positionIndex[min] = i2;
                }
                Vector3 max = sideVertices[i1];
                max.y = 0;
                int i3;
                if (positionIndex.ContainsKey(max))
                {
                    i3 = positionIndex[max];
                }
                else
                {
                    sideVertices.Add(max);
                    sideNormals.Add(new Vector3(0, 0, 1));
                    i3 = sideVertices.Count - 1;
                    positionIndex[max] = i3;
                }

                sideFaces.Add(i0);
                sideFaces.Add(i1);
                sideFaces.Add(i2);
                sideFaces.Add(i2);
                sideFaces.Add(i1);
                sideFaces.Add(i3);
            }
        }
        int vertCount = sideVertices.Count;
        for (int i = 0; i < vertCount; i++)
        {
            sideVertices.Add(sideVertices[i]);
            sideNormals.Add(-sideNormals[i]);
            //uvs.Add(uvs[i]);
        }
        int triCount = sideFaces.Count;
        for (int i = 0; i < triCount; i += 3)
        {
            sideFaces.Add(sideFaces[i + 2] + vertCount);
            sideFaces.Add(sideFaces[i + 1] + vertCount);
            sideFaces.Add(sideFaces[i] + vertCount);
        }

        Mesh sideMesh = new Mesh();
        sideMesh.SetVertices(sideVertices);
        sideMesh.SetNormals(sideNormals);
        sideMesh.SetTriangles(sideFaces, 0);
        return sideMesh;
    }

    protected Mesh TessellateVCurve(float v_min, float v_max, float u)
    {
        float alloweddist = 0.05f;

        var positionIndex = new Dictionary<Vector3, int>();
        mappingCache = new Dictionary<Vector2, Vector3>();

        var sideVertices = new List<Vector3>();
        var sideFaces = new List<int>();
        var sideNormals = new List<Vector3>();

        var indices = new Stack<int>();
        var vMinMax = new Stack<float>();
        int i0, i1;
        float vmin, vmax;
        sideVertices.Add(MapPoint01(u, v_min)); sideVertices.Add(MapPoint01(u, v_max));
        sideNormals.Add(new Vector3(1, 0, 0)); sideNormals.Add(new Vector3(1, 0, 0));
        indices.Push(1); indices.Push(0);
        vMinMax.Push(v_max); vMinMax.Push(v_min);
        positionIndex[MapPoint01(u, v_min)] = 0;
        positionIndex[MapPoint01(u, v_max)] = 1;

        while (indices.Count != 0)
        {
            i0 = indices.Pop(); i1 = indices.Pop();
            vmin = vMinMax.Pop(); vmax = vMinMax.Pop();

            if (OnBoundingBox(sideVertices[i0]) && OnBoundingBox(sideVertices[i1]))
            {
                continue;
            }

            float vmid = (vmin + vmax) / 2.0f;
            Vector3 mid = (sideVertices[i0] + sideVertices[i1]) / 2.0f;
            Vector3 Mid = MapPoint01(u, vmid);
            Vector3 vec025 = (sideVertices[i0] + mid) / 2.0f;
            Vector3 Vec025 = MapPoint01(u, (vmid + vmin) / 2.0f);
            Vector3 vec075 = (sideVertices[i1] + mid) / 2.0f;
            Vector3 Vec075 = MapPoint01(u, (vmid + vmax) / 2.0f);

            if (Vector3.Distance(Mid, mid) > alloweddist || Vector3.Distance(Vec025, vec025) > alloweddist
                || Vector3.Distance(Vec075, vec075) > alloweddist)
            {
                int i2;
                sideVertices.Add(Mid);
                sideNormals.Add(new Vector3(1, 0, 0));
                i2 = sideVertices.Count - 1;
                positionIndex[Mid] = i2;

                indices.Push(i2); indices.Push(i0);
                vMinMax.Push(vmid); vMinMax.Push(vmin);
                indices.Push(i1); indices.Push(i2);
                vMinMax.Push(vmax); vMinMax.Push(vmid);
            }
            else
            {
                Vector3 min = sideVertices[i0];
                min.y = 0;
                int i2;
                if (positionIndex.ContainsKey(min))
                {
                    i2 = positionIndex[min];
                }
                else
                {
                    sideVertices.Add(min);
                    sideNormals.Add(new Vector3(1, 0, 0));
                    i2 = sideVertices.Count - 1;
                    positionIndex[min] = i2;
                }
                Vector3 max = sideVertices[i1];
                max.y = 0;
                int i3;
                if (positionIndex.ContainsKey(max))
                {
                    i3 = positionIndex[max];
                }
                else
                {
                    sideVertices.Add(max);
                    sideNormals.Add(new Vector3(1, 0, 0));
                    i3 = sideVertices.Count - 1;
                    positionIndex[max] = i3;
                }

                sideFaces.Add(i0);
                sideFaces.Add(i1);
                sideFaces.Add(i2);
                sideFaces.Add(i2);
                sideFaces.Add(i1);
                sideFaces.Add(i3);
            }
        }
        int vertCount = sideVertices.Count;
        for (int i = 0; i < vertCount; i++)
        {
            sideVertices.Add(sideVertices[i]);
            sideNormals.Add(-sideNormals[i]);
            //uvs.Add(uvs[i]);
        }
        int triCount = sideFaces.Count;
        for (int i = 0; i < triCount; i += 3)
        {
            sideFaces.Add(sideFaces[i + 2] + vertCount);
            sideFaces.Add(sideFaces[i + 1] + vertCount);
            sideFaces.Add(sideFaces[i] + vertCount);
        }

        Mesh sideMesh = new Mesh();
        sideMesh.SetVertices(sideVertices);
        sideMesh.SetNormals(sideNormals);
        sideMesh.SetTriangles(sideFaces, 0);
        return sideMesh;
    }

    protected Mesh GetBottom(float u, float v)
    {
        List<Vector3> vertices = new List<Vector3>();
        List<Vector3> normals = new List<Vector3>();
        List<Vector2> uvs = new List<Vector2>();
        List<int> faces = new List<int>();

        Vector3 v1 = new Vector3(uMin, 0, vMin);
        Vector3 v2 = new Vector3(uMin, 0, v);
        Vector3 v3 = new Vector3(u, 0, v);
        Vector3 v4 = new Vector3(u, 0, vMin);

        vertices.Add(v1); vertices.Add(v2); vertices.Add(v3); vertices.Add(v4);
        normals.Add(new Vector3(0, 1, 0)); normals.Add(new Vector3(0, 1, 0)); normals.Add(new Vector3(0, 1, 0)); normals.Add(new Vector3(0, 1, 0));
        uvs.Add(new Vector2(0, 0)); uvs.Add(new Vector2(0, 1)); uvs.Add(new Vector2(1, 1)); uvs.Add(new Vector2(1, 0));
        faces.Add(0); faces.Add(1); faces.Add(2); faces.Add(0); faces.Add(2); faces.Add(3);

        vertices.Add(v1); vertices.Add(v2); vertices.Add(v3); vertices.Add(v4);
        normals.Add(new Vector3(0, -1, 0)); normals.Add(new Vector3(0, -1, 0)); normals.Add(new Vector3(0, -1, 0)); normals.Add(new Vector3(0, -1, 0));
        uvs.Add(new Vector2(0, 0)); uvs.Add(new Vector2(0, 1)); uvs.Add(new Vector2(1, 1)); uvs.Add(new Vector2(1, 0));
        faces.Add(4); faces.Add(6); faces.Add(5); faces.Add(4); faces.Add(7); faces.Add(6);

        Mesh mesh = new Mesh();
        mesh.SetVertices(vertices);
        mesh.SetNormals(normals);
        mesh.SetUVs(0, uvs);
        mesh.SetTriangles(faces, 0);
        return mesh;
    }
}
