using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class LoadButton : QuickButton
{

    ExpressionSaveLoad saver;
    public CalcManager calcManager;

    protected override void Start()
    {
        base.Start();
        saver = GetComponent<ExpressionSaveLoad>();
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
        //foreach (List<ExpressionSet> list in saver.LoadExpressions()) {
        //    foreach (ExpressionSet es in list)
        //    {
        //        es.PrintOut();
        //    }
        //}
        //List<ExpressionSet> esl = saver.LoadExpressions()[0].expressionSetSet;

        //ExpressionSet es = esl[0];
        //es.PrintOut();
        //calcManager.expressionSet.expressions[ExpressionSet.ExpOptions.X] = es.expressions[ExpressionSet.ExpOptions.X];
        //calcManager.expressionSet.expressions[ExpressionSet.ExpOptions.Y] = es.expressions[ExpressionSet.ExpOptions.Y];
        //calcManager.expressionSet.expressions[ExpressionSet.ExpOptions.Z] = es.expressions[ExpressionSet.ExpOptions.Z];

        //calcManager.expressionSet.ranges["u"] = es.ranges["u"];
        //calcManager.expressionSet.ranges["v"] = es.ranges["v"];
        //calcManager.PresetPressed();
    }
}
