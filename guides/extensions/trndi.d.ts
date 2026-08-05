/** Type declarations for Trndi's additive Extensions API v2. */

interface TrndiReading {
  value_system: number;
  value_mgdl: number;
  value_mmol: number;
  delta_mgdl: number;
  delta_mmol: number;
  direction: string;
  /** Delphi TDateTime, retained for compatibility with the existing API. */
  timestamp: number;
  age_seconds: number;
}

interface TrndiPrediction {
  0: number;
  1: number;
  2: number;
  3: number;
  length: 4;
}

interface TrndiApiInfo {
  version: "2.0";
  extensionId: string;
  permissions: string[];
  capabilities: { storage: boolean; network: boolean; exec: boolean };
}

declare const Trndi: {
  api: TrndiApiInfo;
  permissions: {
    has(permission: string): boolean;
    require(permission: string): true;
  };
  data: {
    current(): TrndiReading | false;
    readings(options?: { limit?: number }): TrndiReading[];
    limits(): object;
    statistics(options?: { minutes?: number }): object;
    predict(options?: { count?: number }): {
      values: TrndiPrediction[];
      confidence: number;
    };
  };
  net?: {
    fetch(url: string, init?: object): Promise<object>;
  };
  storage?: {
    get(key: string): string | false;
    set(key: string, value: string | number | boolean): void;
    remove(key: string): void;
  };
};