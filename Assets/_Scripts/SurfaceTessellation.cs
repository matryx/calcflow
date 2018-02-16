using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using AK;
using System.Threading;
using System.IO;
using VoxelBusters.RuntimeSerialization;

[RuntimeSerializable(typeof(MonoBehaviour), true, true)]
public class SurfaceTessellation : MonoBehaviour { 
    public struct EquationSet {
        

        public string exprX, exprY, exprZ;
        public float uMin, uMax, vMin, vMax;
    }

    Queue<EquationSet> queue;
    private ExpressionSolver solver;
    AK.Expression expX, expY, expZ;
    Variable varU, varV;
    float uMin, uMax, vMin, vMax;

    List<Vector3> positions;
    List<Vector3> normals;
    List<Vector2> uvs;
    List<int> faces;

    Coroutine tessel;
    bool isRunning;
    [SerializeField] MeshFilter mesh;

    private void Start()
    {
        queue = new Queue<EquationSet>();
        solver = new ExpressionSolver();
        positions = new List<Vector3>();
        normals = new List<Vector3>();
        uvs = new List<Vector2>();
        faces = new List<int>();
        gameObject.SetActive(false);
    }

    private void Update()
    {
        if(queue.Count != 0)
        {
            if(isRunning == false)
            {
                EquationSet es = queue.Dequeue();
                uMin = es.uMin; uMax = es.uMax; vMin = es.vMin; vMax = es.vMax;
                solver.SetGlobalVariable("u", uMin * Mathf.PI);
                solver.SetGlobalVariable("v", vMin * Mathf.PI);
                expX = solver.SymbolicateExpression(es.exprX);
                expY = solver.SymbolicateExpression(es.exprY);
                expZ = solver.SymbolicateExpression(es.exprZ);
                varU = solver.GetGlobalVariable("u");
                varV = solver.GetGlobalVariable("v");

                positions.Clear();
                normals.Clear();
                faces.Clear();
                uvs.Clear();
                mappingCache = new Dictionary<Vector2, Vector3>();

                Vector3 v0 = MapPoint01(0, 0);
                Vector3 n0 = GetNormal01(0, 0);
                positions.Add(v0); normals.Add(n0); uvs.Add(new Vector2(0, 0));
                Vector3 v1 = MapPoint01(0, 1);
                Vector3 n1 = GetNormal01(0, 1);
                positions.Add(v1); normals.Add(n1); uvs.Add(new Vector2(0, 1));
                Vector3 v2 = MapPoint01(1, 1);
                Vector3 n2 = GetNormal01(1, 1);
                positions.Add(v2); normals.Add(n2); uvs.Add(new Vector2(1, 1));
                Vector3 v3 = MapPoint01(1, 0);
                Vector3 n3 = GetNormal01(1, 0);
                positions.Add(v3); normals.Add(n3); uvs.Add(new Vector2(1, 0));
                Tessellate(0, 1, 2, 3);
                //tessel = StartCoroutine(Tessellate(0, 1, 2, 3));
            }
        }
    }

    public void EnqueueEquation(string exprX, string exprY, string exprZ, float uMin, float uMax, float vMin, float vMax)
    {
        EquationSet es;
        es.exprX = exprX;es.exprY = exprY;es.exprZ = exprZ;
        es.uMin = uMin;es.uMax = uMax;es.vMin = vMin;es.vMax = vMax;
        queue.Enqueue(es);
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
        mappingCache[uv] = new Vector3((float)expX.Evaluate(), (float)expZ.Evaluate(), (float)expY.Evaluate());
        return mappingCache[uv];
    }

    Vector3 GetNormal01(float u, float v)
    {
        Vector3 tanU = MapPoint01(u + 0.0001f, v) - MapPoint01(u, v);
        Vector3 tanV = MapPoint01(u, v + 0.0001f) - MapPoint01(u, v);
        return (Vector3.Cross(tanU, tanV) * 10000f).normalized;
    }

    Vector3 Clamp(Vector3 v)
    {
        float x = v.x; x = x <= 20f ? x : 20f; x = x >= -20f ? x : -20f;
        float y = v.y; y = y <= 20f ? y : 20f; y = y >= -20f ? y : -20f;
        float z = v.z; z = z <= 20f ? z : 20f; z = z >= -20f ? z : -20f;
        return new Vector3(x, y, z);
    }

    bool OnBoundingBox(Vector3 v)
    {
        return v.x >= 20f || v.x <= -20f || v.y >= 20f || v.y <= -20f || v.z >= 20f || v.z <= -20f;
    }

    Dictionary<Vector2,Vector3> mappingCache = new Dictionary<Vector2, Vector3>();
    IEnumerator Tessellate(int x, int y, int z, int w, float umin_ = 0f, float umax_ = 1f, float vmin_ = 0f, float vmax_ = 1f)
    {
        isRunning = true;
        float alloweddist = 0.03f;

        var positionIndex = new Dictionary<Vector3, int>();

        int i0, i1, i2, i3;
        float umin, umax, vmin, vmax;
        Stack<int> indices = new Stack<int>();
        Stack<float> uvRanges = new Stack<float>();
        //Queue<int> polygons = new Queue<int>();
        //Queue<float> polygonUVs = new Queue<float>();
        //indices.Push(x); indices.Push(y); indices.Push(z); indices.Push(w);
        //uvRanges.Push(umin_); uvRanges.Push(umax_); uvRanges.Push(vmin_); uvRanges.Push(vmax_);
        float umid_ = (umin_ + umax_) / 2.0f;
        float vmid_ = (vmin_ + vmax_) / 2.0f;
        positions.Add(MapPoint01(umid_, vmid_)); normals.Add(GetNormal01(umid_, vmid_)); uvs.Add(new Vector2(umid_, vmid_));
        int center = positions.Count - 1;
        positions.Add(MapPoint01(umin_, vmid_)); normals.Add(GetNormal01(umin_, vmid_)); uvs.Add(new Vector2(umin_, vmid_));
        int xy = positions.Count - 1;
        positions.Add(MapPoint01(umid_, vmax_)); normals.Add(GetNormal01(umid_, vmax_)); uvs.Add(new Vector2(umid_, vmax_));
        int yz = positions.Count - 1;
        positions.Add(MapPoint01(umax_, vmid_)); normals.Add(GetNormal01(umax_, vmid_)); uvs.Add(new Vector2(umax_, vmid_));
        int zw = positions.Count - 1;
        positions.Add(MapPoint01(umid_, vmin_)); normals.Add(GetNormal01(umid_, vmin_)); uvs.Add(new Vector2(umid_, vmin_));
        int wx = positions.Count - 1;

        indices.Push(x); indices.Push(xy); indices.Push(center); indices.Push(wx);
        uvRanges.Push(umin_); uvRanges.Push(umid_); uvRanges.Push(vmin_); uvRanges.Push(vmid_);
        indices.Push(xy); indices.Push(y); indices.Push(yz); indices.Push(center);
        uvRanges.Push(umin_); uvRanges.Push(umid_); uvRanges.Push(vmid_); uvRanges.Push(vmax_);
        indices.Push(center); indices.Push(yz); indices.Push(z); indices.Push(zw);
        uvRanges.Push(umid_); uvRanges.Push(umax_); uvRanges.Push(vmid_); uvRanges.Push(vmax_);
        indices.Push(wx); indices.Push(center); indices.Push(zw); indices.Push(w);
        uvRanges.Push(umid_); uvRanges.Push(umax_); uvRanges.Push(vmin_); uvRanges.Push(vmid_);

        while (indices.Count != 0)
        {
            //Vector3 tanU = new Vector3(), tanV = new Vector3();
            i3 = indices.Pop(); i2 = indices.Pop(); i1 = indices.Pop(); i0 = indices.Pop();
            vmax = uvRanges.Pop(); vmin = uvRanges.Pop(); umax = uvRanges.Pop(); umin = uvRanges.Pop();

            if(positions.Count > 65500)
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
            if ((OnBoundingBox(v0) && OnBoundingBox(v1)) || (OnBoundingBox(v2) && OnBoundingBox(v3)) || (OnBoundingBox(v0) && OnBoundingBox(v3)) || (OnBoundingBox(v1) && OnBoundingBox(v2)))
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
                faces.Add(i0);
                faces.Add(i1);
                faces.Add(i2);
                faces.Add(i0);
                faces.Add(i2);
                faces.Add(i3);
            }
            //mesh.mesh.Clear();
            //mesh.mesh.SetVertices(positions);
            //mesh.mesh.SetNormals(normals);
            //mesh.mesh.SetUVs(0, uvs);
            //mesh.mesh.SetTriangles(faces, 0);
            //yield return null;
        }


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

        //string filename = System.DateTime.Now.ToString("yyyyMMddHHmmss");
        //ObjExporter.MeshToFile(mesh, filename + ".obj");

        GameObject inst = Instantiate(mesh.gameObject, transform.position, transform.rotation);
        inst.transform.localScale = transform.localScale;
        Mesh newMesh = inst.GetComponent<MeshFilter>().mesh;
        newMesh.SetVertices(positions);
        newMesh.SetNormals(normals);
        newMesh.SetUVs(0, uvs);
        newMesh.RecalculateNormals();
        newMesh.SetTriangles(faces, 0);
        //inst.AddComponent<MeshCollider>();
        //inst.GetComponent<MeshCollider>().sharedMesh = newMesh;
        inst.transform.parent = this.transform;

        string filename = System.DateTime.Now.ToString("yyyyMMddHHmmss");
        string path = System.Environment.GetFolderPath(System.Environment.SpecialFolder.MyDocuments);
        if (!Directory.Exists(Path.Combine(path, "CalcflowExports")));
        {
            Directory.CreateDirectory(Path.Combine(path, "CalcflowExports"));
        }
        ObjExporter.SaveMesh(inst.GetComponent<MeshFilter>(), Path.Combine(Path.Combine(path, "CalcflowExports"), filename + ".obj"));

        mesh.mesh.Clear();
        isRunning = false;
        return null;
    }
}
