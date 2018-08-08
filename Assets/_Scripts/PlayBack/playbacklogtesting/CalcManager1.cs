using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using TMPro;
using VoxelBusters.RuntimeSerialization;

[RuntimeSerializable(typeof(MonoBehaviour), true, true)]
public class CalcManager1 : Nanome.Core.Behaviour
{

    [RuntimeSerializeField]
    public int testint = 100;

    [HideInInspector]
    public bool inputReceived;

    [HideInInspector]
    public ExpressionSet expressionSet;

    [HideInInspector]
    public SaveLoadMenu saveLoadMenu;

    public CustomParametrizedSurface paramSurface;
    [RuntimeSerializeField]
    private CalcInput calcInput;
    private PieceWiseControl pieceWiseControl;
    private BoundsManager boundsManager;
    private Color positiveFeedback = new Color(0, 204, 54);
    private Color negativeFeedback = Color.red;

    int maxDisplayLength = 20;
    int rangeDisplayLength = 5;

    internal bool toExport = false;

    [SerializeField]
    public FlexActionableComponent defaultSpeed;
    public FlexActionableComponent defaultEffect;

    [RuntimeSerializeField]
    [SerializeField]
    ConnectedMenus connectedMenus;

    [RuntimeSerializeField]
    [SerializeField]
    FeedBacks feedbacks;

    [RuntimeSerializeField]
    [SerializeField]
    Inputs inputs;

    [RuntimeSerializable(null, true, true)]
    [System.Serializable]
    internal class ConnectedMenus
    {
        [SerializeField]
        internal CalcInput calcInput;
        [SerializeField]
        internal PieceWiseControl pieceWiseControl;
        [SerializeField]
        internal PresetMenu presetMenu;
        [SerializeField]
        internal OutputMenu outputMenu;
        [SerializeField]
        internal BoundsManager boundsManager;
        [SerializeField]
        internal SaveLoadMenu saveLoadMenu;
        [SerializeField]
        internal ParticleAnimationSettings particleAnimationSettings;
    }

    [RuntimeSerializable(null, true, true)]
    [System.Serializable]
    internal class FeedBacks
    {
        [SerializeField]
        internal Renderer xFeedback;
        [SerializeField]
        internal Renderer yFeedback;
        [SerializeField]
        internal Renderer zFeedback;
        [SerializeField]
        internal Renderer tFeedback;
        [SerializeField]
        internal Renderer uFeedback;
        [SerializeField]
        internal Renderer vFeedback;
        [SerializeField]
        internal Renderer wFeedback;
    }

    [RuntimeSerializable(null, true, true)]
    [System.Serializable]
    internal class Inputs
    {
        [SerializeField]
        internal TextMeshPro xInputbox, yInputbox, zInputbox,
                tMinInput, tMaxInput,
                uMinInput, uMaxInput,
                vMinInput, vMaxInput,
                wMinInput, wMaxInput;
    }

    private void Initialize()
    {
        if (connectedMenus != null)
        {
            if (null != connectedMenus.calcInput)        calcInput        = connectedMenus.calcInput;
            if (null != connectedMenus.boundsManager)    boundsManager    = connectedMenus.boundsManager;
            if (null != connectedMenus.pieceWiseControl) pieceWiseControl = connectedMenus.pieceWiseControl;
            if (null != connectedMenus.boundsManager)    boundsManager    = connectedMenus.boundsManager;
            if (null != connectedMenus.saveLoadMenu)     saveLoadMenu     = connectedMenus.saveLoadMenu;
        }  
    }

    void Awake()
    {
        Initialize();
    }
}
